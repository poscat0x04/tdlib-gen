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

adtI :: ADT -> Q [Dec]
adtI a@ADT {..} =
  let con = mkName $ unpack name
      mapping = snd $ sanitizeADT a
      opt = mkOption (mkModifier mapping)
   in deriveJSON opt con

concatDec :: [Q [Dec]] -> Q [Dec]
concatDec = fmap (concat) . sequence

preProcess :: FilePath -> IO ([ADT], [ADT])
preProcess fp = do
  file <- T.readFile fp
  let mprog = runParser program "td_api.tl" file
  case mprog of
    Left _ -> error "parse failed!"
    Right prog -> do
      let (d, f) = convProgram prog
      let types = fmap (convADT defTyMap) d
      let funs = fmap (convFun defTyMap) f
      let funArgs = fmap paramADT funs
      pure (types, funArgs)

preProcessQ :: Q ([ADT], [ADT])
preProcessQ = runIO (preProcess "data/td_api.tl")

typeInstances :: Q [Dec]
typeInstances = do
  p <- preProcessQ
  concatDec $ fmap adtI $ fst p

funArgInstances :: Q [Dec]
funArgInstances = do
  p <- preProcessQ
  concatDec $ fmap adtI $ snd p
