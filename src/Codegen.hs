module Codegen where

import Data.Aeson
import Data.Char
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Language.Haskell.Codegen
import Language.TL.AST hiding (ADT (..), Ann, App, Type (..))
import qualified Language.TL.AST as A

upper :: Text -> Text
upper t = T.cons (toUpper $ T.head t) (T.tail t)

lower :: String -> String
lower t = (toLower $ head t) : (tail t)

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
mkModifier m s = fromMaybe s (M.lookup s m)

defTyMap :: TyMap
defTyMap =
  M.fromList
    [ ("vector", "[]"),
      ("string", "T"),
      ("int32", "I32"),
      ("int64", "I64"),
      ("int53", "I53"),
      ("bytes", "ByteString64"),
      ("Ok", "()")
    ]

typeConv :: TyMap -> A.Type -> Type
typeConv m (A.Type t) = Type $ fromMaybe (upper t) $ M.lookup t m
typeConv m (A.TypeApp t ts) = app (typeConv m t) (fmap (typeConv m) ts)
typeConv m A.NatType = Type "Int"

app :: Type -> [Type] -> Type
app t = foldl (\acc ty -> App acc ty) t

convArg :: TyMap -> Int -> Arg -> (Field, (String, String))
convArg m i Arg {..} =
  let newName = argName <> "_" <> pack (show i)
   in ( Field
          { name = newName,
            ty = typeConv m argType,
            ..
          },
        (unpack newName, unpack argName)
      )

convArg' :: TyMap -> Arg -> Field
convArg' m Arg {..} =
  Field
    { name = argName,
      ty = typeConv m argType,
      ..
    }

combToConstr :: TyMap -> Int -> Combinator -> (Constr, FieldMapping)
combToConstr m i Combinator {..} =
  let (fields, l) = unzip $ fmap (convArg m i) args
   in ( Constr
          { name = upper ident,
            ..
          },
        M.fromList l
      )

combToConstr' :: TyMap -> Combinator -> Constr
combToConstr' m Combinator {..} =
  Constr
    { name = upper ident,
      fields = fmap (convArg' m) args,
      ..
    }

formArr :: [Field] -> Type -> Ann -> TypeSig
formArr fields resT resAnn = foldl (\res Field {..} -> Conn {..}) (Result {ty = resT, ann = resAnn}) fields

combToFun :: TyMap -> Combinator -> FunDef
combToFun m Combinator {..} =
  FunDef
    { name = ident,
      typeSig = formArr (fmap (convArg' m) args) (typeConv m resType) Nothing,
      ..
    }

convADT :: TyMap -> A.ADT -> ADT
convADT m A.ADT {..} =
  let (constr, mappings) = unzip $ fmap (uncurry (combToConstr m)) $ zip [1 ..] constructors
      mapping = fold mappings
   in ADT
        { ..
        }

convFun :: TyMap -> Function -> FunDef
convFun m (Function c) = combToFun m c
