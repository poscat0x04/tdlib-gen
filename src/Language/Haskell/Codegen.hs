{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Language.Haskell.Codegen where

import Control.Lens
import Data.Generics.Labels ()
import Data.List
import Data.Map.Strict (Map)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Internal
import GHC.Generics

type Ann = Maybe Text

io :: Type
io = Type "IO"

prettyDoc :: Ann -> Doc ann
prettyDoc (Just d) =
  let ls = T.lines d
      ds = fmap unsafeTextWithoutNewlines ls
   in "-- |" <+> foldl1 (<>) (intersperse (line <> "--" <> line <> "--") ds) <> line
prettyDoc Nothing = mempty

data ADT
  = ADT
      { name :: Text,
        ann :: Ann,
        constr :: [Constr],
        mapping :: Map String String
      }
  deriving (Show, Eq, Generic)

prettyConstrs :: [Doc ann] -> Doc ann
prettyConstrs [] = mempty
prettyConstrs (x : xs) =
  vsep
    [ foldl1 (<>) (intersperse (line <> "| ") (("=" <+> x) : xs)),
      "deriving (Show, Eq, Generic)"
    ]

instance Pretty ADT where
  pretty ADT {..} =
    let doc = prettyDoc ann
        n = unsafeTextWithoutNewlines name
        cs = fmap pretty constr
     in doc
          <> vsep
            [ "data" <+> n,
              indent 2 (prettyConstrs cs)
            ]

-- >>> pretty $ ADT "A" (Just "A") [Constr "A" Nothing [("a", Type "A"), ("b", Type "A")], Constr "B" Nothing []]

data Field
  = Field
      { name :: Text,
        ann :: Ann,
        ty :: Type
      }
  deriving (Show, Eq, Generic)

instance Pretty Field where
  pretty Field {..} =
    prettyDoc ann
      <> unsafeTextWithoutNewlines name <+> "::" <+> pretty ty

data Constr
  = Constr
      { name :: Text,
        ann :: Ann,
        fields :: [Field]
      }
  deriving (Show, Eq, Generic)

arity :: Constr -> Int
arity = length . fields

instance Pretty Constr where
  pretty Constr {..} =
    let doc = prettyDoc ann
        n = unsafeTextWithoutNewlines name
        fs = foldl (<>) mempty (intersperse ("," <> line) (fmap pretty fields))
     in doc
          <> vsep
            [ n,
              indent 2 ("{" <+> align fs <> line <> "}")
            ]

data Type
  = Type Text
  | Arr Type Type
  | App Type Type
  deriving (Show, Eq, Generic)

instance Pretty Type where
  pretty (Type t) = unsafeTextWithoutNewlines t
  pretty (Arr ty ty') = pretty ty <+> "->" <+> pretty ty'
  pretty (App (Type "[]") ty) = "[" <> pretty ty <> "]"
  pretty (App tyCon ty) = "(" <> pretty tyCon <> ")" <+> "(" <> pretty ty <> ")"

data TypeSig
  = Result
      { ty :: Type,
        ann :: Ann
      }
  | Conn
      { ty :: Type,
        ann :: Ann,
        res :: TypeSig
      }
  deriving (Show, Eq, Generic)

instance Pretty TypeSig where
  pretty (Result ty doc) =
    prettyDoc doc <> "Sem r" <+> "(" <> "Error ∪" <+> pretty ty <> ")"
  pretty (Conn ty doc res) =
    prettyDoc doc
      <> vsep
        [ pretty ty <+> "->",
          pretty res
        ]

type Annotated = (Type, Ann)

formArr :: [Annotated] -> Annotated -> TypeSig
formArr [] (ty, ann) = Result ty ann
formArr ((ty, ann) : xs) a = Conn ty ann (formArr xs a)

data FunDef
  = FunDef
      { name :: Text,
        ann :: Ann,
        constr :: Constr,
        res :: Type
      }
  deriving (Show, Eq, Generic)

getAnn :: Field -> Annotated
getAnn Field {..} = (ty, ann)

flattenSig :: FunDef -> Doc ann
flattenSig FunDef {..} =
  let n = unsafeTextWithoutNewlines name
      doc = prettyDoc ann
      c = unsafeTextWithoutNewlines (constr ^. #name)
      sig = pretty $ formArr (fmap getAnn (fields constr)) (res, Nothing)
   in vsep
        [ doc <> n <+> "::",
          indent 2 "Member TDLib r =>",
          indent 2 sig
        ]

vars :: Int -> Doc ann
vars i = hsep $ fmap (fromString . ("_" <>) . show) [1 .. i]

flattenBody :: FunDef -> Doc ann
flattenBody FunDef {..} =
  let n = unsafeTextWithoutNewlines name
      c = unsafeTextWithoutNewlines (constr ^. #name)
      ar = arity constr
      v = vars ar
   in hsep [n, v, "=", "runCmd $", c, v]

flattenPrint :: FunDef -> Doc ann
flattenPrint def =
  vsep
    [ flattenSig def,
      flattenBody def
    ]

simplePretty :: FunDef -> Doc ann
simplePretty FunDef {..} =
  let doc = prettyDoc ann
      n = unsafeTextWithoutNewlines name
      cmd = unsafeTextWithoutNewlines (constr ^. #name)
      resTy = pretty res
   in doc
        <> vsep
          [ n <+> "::" <+> "Member TDLib r" <+> "=>" <+> cmd <+> "->" <+> "Sem r (Error ∪ " <> resTy <> ")",
            n <+> "=" <+> "runCmd"
          ]

instance Pretty FunDef where
  pretty d@FunDef {..} = flattenPrint d
