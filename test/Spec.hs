module Main where

import Codegen
import qualified Data.Text.IO as T
import Language.Haskell.Codegen
import Language.TL.AST
import Language.TL.Parser
import Processing
import Text.Megaparsec
import Text.Pretty.Simple

main :: IO ()
main = do
  f <- T.readFile "test/data/td_api.tl"
  let Right prog = runParser program "" f
  let (datas, functions) = convProgram prog
  let adts = fmap (convADT defTyMap) datas
  let p = fmap sanitizeADT adts
  pPrint p
