{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.Codegen.TH where

import Codegen
import Data.Aeson
import Data.Aeson.TH
import Data.Text (pack, unpack)
import qualified Data.Text.IO as T
import Instances.TH.Lift
import Language.Haskell.Codegen
import Language.Haskell.TH
import Language.TL.Parser
import Processing
import Text.Megaparsec

adtInstanceDec :: ADT -> Q [Dec]
adtInstanceDec ADT {..} =
  let con = mkName $ unpack name
      opt = mkOption (mkModifier mapping)
   in deriveJSON opt con

concatDec :: [Q [Dec]] -> Q [Dec]
concatDec = fmap (concat) . sequence

genDec :: FilePath -> Q [Dec]
genDec fp = do
  f <- runIO $ T.readFile fp
  let Right prog = runParser program "td_api.tl" f
  let (datas, funs) = convProgram prog
  concatDec $ fmap adtInstanceDec $ fmap (convADT defTyMap) datas

dataDec :: Q [Dec]
dataDec = genDec "test/data/td_api.tl"

genFunDef :: TyMap -> FunDef -> Q [Dec]
genFunDef m d = undefined
