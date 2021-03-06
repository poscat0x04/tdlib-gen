module Codegen where

import Data.Aeson
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, isInfixOf, pack, unpack)
import qualified Data.Text as T
import Language.Haskell.Codegen
import Language.TL.AST hiding (ADT (..), Ann, App, Type (..))
import qualified Language.TL.AST as A
import Text.Casing

type TyMap = Map Text Text

type FieldMapping = Map String String

type Modifier = String -> String

mkOption ::
  -- | field modifier
  Modifier ->
  Options
mkOption fieldM =
  defaultOptions
    { fieldLabelModifier = fieldM,
      constructorTagModifier = lower,
      sumEncoding =
        TaggedObject
          { tagFieldName = "@type",
            contentsFieldName = error "Not a record"
          },
      tagSingleConstructors = True,
      allNullaryToStringTag = False
    }

mkModifier :: FieldMapping -> Modifier
mkModifier m s = fromMaybe (camelToSnake s) (M.lookup s m)

defTyMap :: TyMap
defTyMap =
  M.fromList
    [ ("vector", "[]"),
      ("string", "T"),
      ("int32", "I32"),
      ("int64", "I64"),
      ("int53", "I53"),
      ("bytes", "ByteString64")
    ]

typeConv :: TyMap -> A.Type -> Type
typeConv m (A.Type t) = Type $ fromMaybe (upper t) $ M.lookup t m
typeConv m (A.TypeApp t ts) = app (typeConv m t) (fmap (typeConv m) ts)
typeConv _ A.NatType = Type "Int"

app :: Type -> [Type] -> Type
app t = foldr (\ty acc -> App acc ty) t

convArg :: TyMap -> Arg -> Field
convArg m Arg {..} =
  let ty = typeConv m argType
   in Field
        { name = argName,
          ty = case ann of
            Nothing -> ty
            Just doc ->
              if "may be null" `isInfixOf` doc
                then warpM ty
                else ty,
          ..
        }

combToConstr :: TyMap -> Combinator -> Constr
combToConstr m Combinator {..} =
  Constr
    { name = upper ident,
      fields = fmap (convArg m) args,
      ..
    }

formArr :: [Field] -> Type -> Ann -> TypeSig
formArr fields resT resAnn = foldr (\Field {..} res -> Conn {..}) (Result {ty = resT, ann = resAnn}) fields

combToFun :: TyMap -> Combinator -> FunDef
combToFun m c@Combinator {..} =
  FunDef
    { name = ident,
      constr = combToConstr m c,
      res = typeConv m resType,
      ..
    }

convADT :: TyMap -> A.ADT -> ADT
convADT m A.ADT {..} =
  let constr = fmap (combToConstr m) constructors
   in ADT
        { ..
        }

countElem :: Eq a => a -> [a] -> Int
countElem _ [] = error "Not in list"
countElem a (x : xs) =
  if x == a
    then 0
    else 1 + countElem a xs

sanitize' :: Text -> (Text, FieldMapping)
sanitize' "type" = ("type_", M.fromList [("type_", "type")])
sanitize' "data" = ("data_", M.fromList [("data_", "data")])
sanitize' "pattern" = ("pattern_", M.fromList [("pattern_", "pattern")])
sanitize' t = (snakeToCamel t, mempty)

sanitizeArg :: Int -> Text -> (Text, FieldMapping)
sanitizeArg 0 t = sanitize' t
sanitizeArg i t =
  let n = (snakeToCamel t) <> "_" <> pack (show i)
   in (n, M.fromList [(unpack n, unpack t)])

type SanitizerState = (Map Text [Type], FieldMapping)

sanitizeField :: SanitizerState -> Field -> (SanitizerState, Field)
sanitizeField (tyMap, fieldMap) Field {..} =
  case M.lookup name tyMap of
    Nothing ->
      let (name', dfm) = sanitizeArg 0 name
       in ((M.insert name [ty] tyMap, fieldMap <> dfm), Field {name = name', ..})
    Just l ->
      if ty `elem` l
        then
          let c = countElem ty l
              (name', dfm) = sanitizeArg c name
           in ((tyMap, fieldMap <> dfm), Field name' ann ty)
        else
          let c = length l + 1
              (name', dfm) = sanitizeArg c name
              tyMap' = M.insert name (l <> [ty]) tyMap
           in ((tyMap', fieldMap <> dfm), Field name' ann ty)

sanitizeField' :: Field -> (SanitizerState, [Field]) -> (SanitizerState, [Field])
sanitizeField' f (s, fs) =
  let (s', f') = sanitizeField s f
   in (s', f' : fs)

sanitizeConstr :: SanitizerState -> Constr -> (SanitizerState, Constr)
sanitizeConstr s Constr {..} =
  let (s', fields') = foldr sanitizeField' (s, []) fields
   in ( s',
        Constr
          { fields = fields',
            ..
          }
      )

sanitizeConstr' :: Constr -> (SanitizerState, [Constr]) -> (SanitizerState, [Constr])
sanitizeConstr' c (s, cs) =
  let (s', c') = sanitizeConstr s c
   in (s', c' : cs)

sanitizeADT :: ADT -> (ADT, FieldMapping)
sanitizeADT ADT {..} =
  let ((_, fm), constr') = foldr sanitizeConstr' mempty constr
   in ( ADT
          { constr = constr',
            ..
          },
        fm
      )

convFun :: TyMap -> Function -> FunDef
convFun m (Function c) = combToFun m c

paramADT :: FunDef -> ADT
paramADT FunDef {..} =
  ADT
    { ann = Just ("Parameter of Function " <> name),
      constr = [constr],
      name = upper name
    }
