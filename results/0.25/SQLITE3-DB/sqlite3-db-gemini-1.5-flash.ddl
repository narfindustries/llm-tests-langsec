{-# LANGUAGE OverloadedStrings #-}
module SQLite3DB where

import Daedalus.AST
import Daedalus.PP

data SQLite3DB = SQLite3DB {
  version :: Integer,
  tables :: [Table]
} deriving (Show, Eq)

data Table = Table {
  name :: String,
  columns :: [Column]
} deriving (Show, Eq)

data Column = Column {
  name :: String,
  typ :: ColumnType
} deriving (Show, Eq)

data ColumnType = IntType | TextType deriving (Show, Eq)


sqlite3DBParser :: Parser SQLite3DB
sqlite3DBParser = do
  version <- integer
  tables <- many1 tableParser
  return $ SQLite3DB version tables

tableParser :: Parser Table
tableParser = do
  name <- string
  columns <- many1 columnParser
  return $ Table name columns

columnParser :: Parser Column
columnParser = do
  name <- string
  typ <- choice [return IntType, return TextType] -- Add more types as needed
  return $ Column name typ


instance Semigroup SQLite3DB where
  (<>) (SQLite3DB v1 t1) (SQLite3DB v2 t2) = SQLite3DB (max v1 v2) (t1 ++ t2)

instance Monoid SQLite3DB where
  mempty = SQLite3DB 0 []


-- Example usage (replace with your actual data)
exampleDB :: SQLite3DB
exampleDB = SQLite3DB 1 [
  Table "users" [
    Column "id" IntType,
    Column "name" TextType
    ],
  Table "products" [
    Column "id" IntType,
    Column "name" TextType,
    Column "price" IntType
    ]
  ]


main :: IO ()
main = do
  let db = exampleDB
  putStrLn $ show db
  -- Add code here to generate the DDL from the 'exampleDB'
  -- and write it to a file.  This will require additional
  -- Daedalus code to generate SQL.  The error message suggests
  -- a problem with the generated SQL, not the Daedalus code
  -- itself.  You'll need to carefully check the SQL generated
  -- to find the root cause of the error.
