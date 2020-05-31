module Processing where

import Data.Either (rights)
import Data.List (partition)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Language.TL.AST
import qualified Language.TL.AST as A
import Language.TL.Comment
import Language.TL.Types
import qualified Language.TL.Types as T
import Text.Megaparsec

getComment :: Comment -> Text
getComment (LineComment t) = t
getComment (BlockComment t) = t

type CombD = ([[Attr]], Combinator)

parseComments :: [Comment] -> [[Attr]]
parseComments = rights . fmap (runParser attrs "" . getComment)

hasClassAttr :: [Attr] -> Bool
hasClassAttr = any (\(t, _) -> t == "class")

classDesc :: [[Attr]] -> Maybe Text
classDesc l =
  let (t, _) = partition hasClassAttr l
      l' = if null t then [] else head t
      desc = lookup "description" l'
   in desc

constructDoc :: [[Attr]] -> Doc
constructDoc = M.fromList . concat

tyConName :: Type -> Text
tyConName (A.Type t) = t
tyConName (A.TypeApp (A.Type t) _) = t
tyConName _ = error "not a type constructor"

combType :: Combinator -> Text
combType = tyConName . A.resType

splitDecls :: [CombD] -> ([CombD], [CombD])
splitDecls decls =
  let (_, c) = head decls
      tyIdent = combType c
   in break (\(_, c') -> combType c' /= tyIdent) decls

formADT :: [CombD] -> ADT
formADT decls =
  let (l, c) = head decls
      ann = classDesc l
      name = combType c
      constructors = fmap snd decls
   in ADT {..}

oneADT :: [CombD] -> (ADT, [CombD])
oneADT decls =
  let (h, rest) = splitDecls decls
   in (formADT h, rest)

filterAnnDecl :: [AnnDecl] -> [CombD]
filterAnnDecl [] = []
filterAnnDecl (AnnDecl cmt (T.Combinator c) : xs) =
  let l = parseComments cmt
   in (l, combConv (constructDoc l) c) : filterAnnDecl xs
filterAnnDecl (_ : xs) = filterAnnDecl xs

declBlockToADT :: [CombD] -> [ADT]
declBlockToADT [] = []
declBlockToADT decls =
  let (h, rest) = oneADT decls
   in h : declBlockToADT rest

declBlockToFun :: [CombD] -> [Function]
declBlockToFun = fmap (\(_, c) -> Function c)

convProgram :: Program -> ([ADT], [Function])
convProgram prog =
  foldl
    ( \(c, f) blk -> case blk of
        FunDeclBlk decls -> (c, (declBlockToFun $ filterAnnDecl decls) <> f)
        TypeDeclBlk decls -> ((declBlockToADT $ filterAnnDecl decls) <> c, f)
    )
    ([], [])
    prog
