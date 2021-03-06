module Main where

import Codegen
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Internal
import Language.TL.Parser
import Processing
import Text.Megaparsec

dataDeclHeader :: Doc ann
dataDeclHeader =
  vsep
    [ "{-# LANGUAGE DeriveGeneric #-}",
      "{-# LANGUAGE DeriveAnyClass #-}",
      "{-# LANGUAGE DerivingStrategies #-}",
      "{-# LANGUAGE DuplicateRecordFields #-}",
      "{-# LANGUAGE TemplateHaskell #-}",
      "",
      "-- | TD API data types generated by tdlib-gen",
      "module TDLib.Generated.Types where",
      "",
      "import GHC.Generics",
      "import Language.Haskell.Codegen.TH",
      "import Data.ByteString.Base64.Type",
      "import qualified Data.Text as T",
      "import Language.TL.I64",
      "",
      "type I53 = Int",
      "type I32 = Int",
      "type T = T.Text",
      ""
    ]

funArgHeader :: Doc ann
funArgHeader =
  vsep
    [ "{-# LANGUAGE DeriveGeneric #-}",
      "{-# LANGUAGE DeriveAnyClass #-}",
      "{-# LANGUAGE DerivingStrategies #-}",
      "{-# LANGUAGE DuplicateRecordFields #-}",
      "{-# LANGUAGE TemplateHaskell #-}",
      "",
      "-- | TD API function call arguments",
      "module TDLib.Generated.FunArgs where",
      "",
      "import Data.ByteString.Base64.Type",
      "import GHC.Generics",
      "import Language.Haskell.Codegen.TH",
      "import Language.TL.I64",
      "import TDLib.Generated.Types",
      ""
    ]

funHeader :: Doc ann
funHeader =
  vsep
    [ "{-# LANGUAGE TypeOperators #-}",
      "-- | TD API functions (methods) generated by tdlib-gen",
      "",
      "module TDLib.Generated.Functions where",
      "",
      "import Data.ByteString.Base64.Type",
      "import Language.TL.I64",
      "import Polysemy",
      "import TDLib.Effect",
      "import TDLib.Generated.FunArgs",
      "import TDLib.Generated.Types",
      "import TDLib.Types.Common",
      ""
    ]

main :: IO ()
main = do
  putStrLn "tl file path:"
  fp <- getLine
  f <- T.readFile fp
  let mprog = runParser program "td_api.tl" f
  case mprog of
    Left _ -> error "parse failed"
    Right prog -> do
      let (datas, functions) = convProgram prog
      let adts = fmap (fst . sanitizeADT . convADT defTyMap) datas
      let funDefs = fmap (convFun defTyMap) functions
      let adts' = fmap (fst . sanitizeADT . paramADT) funDefs
      let file1 = dataDeclHeader <> "\n\n" <> vsep (fmap pretty adts) <> "\n\n" <> "typeInstances"
      let file2 = funHeader <> "\n\n" <> vsep (fmap pretty funDefs)
      let file3 = funArgHeader <> "\n\n" <> vsep (fmap pretty adts') <> "\n\n" <> "funArgInstances"
      writeFile "Types.hs" (show file1)
      writeFile "Functions.hs" (show file2)
      writeFile "FunArgs.hs" (show file3)
