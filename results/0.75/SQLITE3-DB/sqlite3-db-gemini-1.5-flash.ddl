{-# LANGUAGE OverloadedStrings #-}
module SQLite3DB where

import Daedalus.Value
import Daedalus.AST

data SQLite3DB = SQLite3DB {
  version :: Integer,
  tables :: [Table]
}

data Table = Table {
  name :: String,
  columns :: [Column]
}

data Column = Column {
  name :: String,
  typ :: Type
}

data Type = IntType | TextType | BlobType

instance Semigroup Table where
  (<>) (Table n1 c1) (Table n2 c2) = Table (n1 ++ n2) (c1 ++ c2)

instance Monoid Table where
  mempty = Table "" []

instance Semigroup SQLite3DB where
  (<>) (SQLite3DB v1 t1) (SQLite3DB v2 t2) = SQLite3DB (v1 + v2) (t1 ++ t2)

instance Monoid SQLite3DB where
  mempty = SQLite3DB 0 []


-- Sample Data
sampleDB :: SQLite3DB
sampleDB = SQLite3DB 1 [
  Table "users" [
    Column "id" IntType,
    Column "name" TextType,
    Column "data" BlobType
    ],
  Table "items" [
    Column "item_id" IntType,
    Column "user_id" IntType,
    Column "description" TextType
    ]
  ]


-- Daedalus grammar to encode/decode SQLite3DB
sqlite3DBGrammar :: Grammar SQLite3DB
sqlite3DBGrammar = do
  version' <- integer
  tables' <- many tableGrammar
  return $ SQLite3DB version' tables'

tableGrammar :: Grammar Table
tableGrammar = do
  tableName <- text
  columns' <- many columnGrammar
  return $ Table tableName columns'

columnGrammar :: Grammar Column
columnGrammar = do
  columnName <- text
  columnType <- choice [
      return IntType,
      return TextType,
      return BlobType
      ]
  return $ Column columnName columnType


main :: IO ()
main = do
  let encoded = encode (Just sampleDB) sqlite3DBGrammar
  case encoded of
    Just x -> putStrLn $ "Encoded: " ++ show x
    Nothing -> putStrLn "Encoding failed"
  let decoded = decode sqlite3DBGrammar (show sampleDB)
  case decoded of
    Just x -> putStrLn $ "Decoded: " ++ show x
    Nothing -> putStrLn "Decoding failed"

