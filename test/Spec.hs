module Main where

import Codegen
import Data.List (intercalate)
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc
import Language.Haskell.Codegen
import Language.TL.Parser
import Processing
import Text.Megaparsec
import Text.Pretty.Simple

main :: IO ()
main = do
  f <- T.readFile "test/data/td_api.tl"
  let Right prog = runParser program "td_api.tl" f
  let (datas, funs) = convProgram prog
  let convertedDatas = fmap (convADT defTyMap) datas
  let convertedFuns = fmap (convFun defTyMap) funs
  let datas = intercalate "\n\n" $ fmap (show . pretty) convertedDatas
  writeFile "Generated.hs" $ show (moduleHeader "Generated") <> "\n\n" <> datas
-- mapM_ (\t -> putStrLn t >> putStrLn "--------") $ fmap (show . pretty) convertedFuns
