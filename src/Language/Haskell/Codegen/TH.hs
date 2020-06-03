{-# LANGUAGE TemplateHaskell #-}

-- | Generate 'ToJSON'/'FromJSON' instances using template haskell
module Language.Haskell.Codegen.TH where

import Codegen
import Data.Aeson.TH
import Data.Text (unpack)
import qualified Data.Text.IO as T
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
  adts <- runIO $ do
    f <- T.readFile fp
    let mprog = runParser program "td_api.tl" f
    case mprog of
      Left _ -> error "parse failed"
      Right prog -> do
        let (datas, functions) = convProgram prog
        let adts = fmap (convADT defTyMap) datas
        let funDefs = fmap (convFun defTyMap) functions
        pure adts
  concatDec $ fmap adtInstanceDec adts

genDec' :: FilePath -> Q [Dec]
genDec' fp = do
  adts <- runIO $ do
    f <- T.readFile fp
    let mprog = runParser program "td_api.tl" f
    case mprog of
      Left _ -> error "parse failed"
      Right prog -> do
        let (datas, functions) = convProgram prog
        let adts = fmap (convADT defTyMap) datas
        let funDefs = fmap (convFun defTyMap) functions
        pure (fmap paramADT funDefs)
  concatDec $ fmap adtInstanceDec adts

instancesDec :: Q [Dec]
instancesDec = genDec "data/td_api.tl"

instancesDec' :: Q [Dec]
instancesDec' = genDec' "data/td_api.tl"

genFunDef :: TyMap -> FunDef -> Q [Dec]
genFunDef m d = undefined
