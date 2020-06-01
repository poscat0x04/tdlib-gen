{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Language.Haskell.Codegen where

import Control.Lens
import Data.Generics.Labels ()
import Data.List
import Data.Map.Strict (Map)
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
  pretty (App tyCon ty) = "(" <> pretty tyCon <> ")" <+> "(" <> pretty ty <> ")"

data TypeSig
  = Result
      { ty :: Type,
        ann :: Ann
      }
  | Conn
      { ty :: Type,
        ann :: Ann,
        name :: Text,
        res :: TypeSig
      }
  deriving (Show, Eq, Generic)

instance Pretty TypeSig where
  pretty (Result ty doc) =
    prettyDoc doc <> pretty (App io ty)
  pretty (Conn ty doc _ res) =
    prettyDoc doc
      <> vsep
        [ pretty ty <+> "->",
          pretty res
        ]

data FunDef
  = FunDef
      { name :: Text,
        ann :: Ann,
        constr :: Constr,
        res :: Type
      }
  deriving (Show, Eq, Generic)

instance Pretty FunDef where
  pretty FunDef {..} =
    let doc = prettyDoc ann
        n = unsafeTextWithoutNewlines name
        cmd = unsafeTextWithoutNewlines (constr ^. #name)
        resTy = pretty res
     in doc
          <> vsep
            [ n <+> "::" <+> "Member TDLib r" <+> "=>" <+> cmd <+> "->" <+> "Sem r (Error :+: " <> resTy <> ")",
              n <+> "=" <+> "runCmd"
            ]
