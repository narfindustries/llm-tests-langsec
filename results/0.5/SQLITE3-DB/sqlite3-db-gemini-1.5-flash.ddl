{-# LANGUAGE OverloadedStrings #-}
module SQLite3DB where

import Daedalus.AST
import Daedalus.PP

data Gemini = Gemini {
  version :: Integer,
  entries :: [Entry]
} deriving (Show, Eq)

data Entry = Entry {
  key :: String,
  value :: String
} deriving (Show, Eq)


instance Semigroup Gemini where
  Gemini v1 e1 <> Gemini v2 e2 = Gemini (max v1 v2) (e1 ++ e2)

instance Monoid Gemini where
  mempty = Gemini 0 []

sqlite3DB :: Daedalus.AST.Value
sqlite3DB = do
  let version = 1
  entries <- many (Entry <$> some (char 'a' -- example, replace with actual key parsing) <*> some (char 'b' -- example, replace with actual value parsing))
  return $ Gemini version entries


main :: IO ()
main = do
  let compiled = compile sqlite3DB
  putStrLn $ show compiled
