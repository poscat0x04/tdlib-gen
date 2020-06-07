module Text.Casing where

import Data.Char
import Data.List (intercalate)
import Data.Text (Text, split)
import qualified Data.Text as T

upper :: Text -> Text
upper t = T.cons (toUpper $ T.head t) (T.tail t)

upper' :: String -> String
upper' (x : xs) = toUpper x : xs

lower :: String -> String
lower t = (toLower $ head t) : (tail t)

lower' :: Text -> Text
lower' t = T.cons (toLower $ T.head t) (T.tail t)

splitOn' :: (Char -> Bool) -> String -> [String]
splitOn' p t = go t
  where
    go1 s "" = (reverse s, "")
    go1 "" (x : xs) = go1 [x] xs
    go1 s s'@(x : xs) = if not (p x) then go1 (x : s) xs else (reverse s, s')
    go x =
      let (s, rest) = go1 "" x
       in if rest == "" then [s] else s : go rest

snakeToCamel :: Text -> Text
snakeToCamel t =
  let (x : xs) = filter (/= "") $ split (== '_') t
   in T.concat (lower' x : fmap upper xs)

camelToSnake :: String -> String
camelToSnake t =
  let l = splitOn' isUpper t
   in intercalate "_" (fmap lower l)
