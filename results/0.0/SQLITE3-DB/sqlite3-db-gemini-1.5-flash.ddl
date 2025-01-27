{-# LANGUAGE OverloadedStrings #-}
module SQLite3DB where

import Daedalus.AST
import Daedalus.PP

data GeminiVersion = GeminiV1_5

data SQLite3DB = SQLite3DB
  { version :: GeminiVersion
  , tables :: [Table]
  }

data Table = Table
  { tableName :: String
  , columns :: [Column]
  }

data Column = Column
  { columnName :: String
  , columnType :: ColumnType
  }

data ColumnType = IntType | TextType | BlobType

instance Semigroup SQLite3DB where
  SQLite3DB v1 t1 <> SQLite3DB v2 t2 = SQLite3DB v1 (t1 ++ t2)

instance Monoid SQLite3DB where
  mempty = SQLite3DB GeminiV1_5 []

-- Example usage:
exampleDB :: SQLite3DB
exampleDB = mconcat
  [ SQLite3DB GeminiV1_5 [Table "users" [Column "id" IntType, Column "name" TextType]]
  , SQLite3DB GeminiV1_5 [Table "items" [Column "item_id" IntType, Column "description" TextType, Column "data" BlobType]]
  ]


-- Daedalus code generation
generateDDL :: SQLite3DB -> String
generateDDL (SQLite3DB _ tables) = unlines $
  [ "PRAGMA foreign_keys = ON;"
  ] ++
  map generateTableDDL tables

generateTableDDL :: Table -> String
generateTableDDL (Table tableName cols) =
  unlines $
  [ "CREATE TABLE IF NOT EXISTS " ++ tableName ++ " ("
  ] ++
  map generateColumnDDL cols ++
  [ ");"
  ]

generateColumnDDL :: Column -> String
generateColumnDDL (Column columnName columnType) =
  columnName ++ " " ++ generateColumnTypeDDL columnType

generateColumnTypeDDL :: ColumnType -> String
generateColumnTypeDDL IntType = "INTEGER"
generateColumnTypeDDL TextType = "TEXT"
generateColumnTypeDDL BlobType = "BLOB"


main :: IO ()
main = do
  let ddl = generateDDL exampleDB
  putStrLn ddl

