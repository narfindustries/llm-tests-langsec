module SQLite3 where

import Daedalus.AST

data PageType = Leaf | Interior | Free deriving (Show, Eq, Bounded, Enum, Daedalus.Type.Hashable)

data SQLiteHeader = SQLiteHeader
  { magic :: String
  , pageSize :: Integer
  , writeCount :: Integer
  , pageSize2 :: Integer
  , version :: Integer
  } deriving (Show, Eq, Daedalus.Type.Hashable)

data PageHeader = PageHeader
  { pageNumber :: Integer
  , pageType :: PageType
  , freeBlockCount :: Integer
  , checksum :: Integer
  } deriving (Show, Eq, Daedalus.Type.Hashable)

data Cell = Cell { cellData :: Bytes } deriving (Show, Eq, Daedalus.Type.Hashable)

data Row = Row { rowId :: Integer, cells :: [Cell] } deriving (Show, Eq, Daedalus.Type.Hashable)

data TableSchema = TableSchema { tableName :: String, columns :: [(String, ColumnType)] } deriving (Show, Eq, Daedalus.Type.Hashable)

data IndexEntry = IndexEntry { indexKey :: [Integer], rowId :: Integer } deriving (Show, Eq, Daedalus.Type.Hashable)

data ColumnType = INT | TEXT | REAL | BLOB | NULL deriving (Show, Eq, Bounded, Enum, Daedalus.Type.Hashable)


data SQLiteDatabase = SQLiteDatabase
  { header :: SQLiteHeader
  , pages :: [Page]
  } deriving (Show, Eq, Daedalus.Type.Hashable)

data Page = Page
 { header :: PageHeader
 , cells :: [Cell]
 , rows :: Maybe [Row]
 , children :: Maybe [Integer]
 , tableSchema :: Maybe TableSchema
 , indexEntries :: Maybe [IndexEntry]
 } deriving (Show, Eq, Daedalus.Type.Hashable)


sqlite3 :: Daedalus.AST.Type
sqlite3 = Daedalus.AST.TApp (Daedalus.AST.TCon "Record") [Daedalus.AST.TRecord [("header", tSQLiteHeader), ("pages", tPages)]]

tSQLiteHeader :: Daedalus.AST.Type
tSQLiteHeader = Daedalus.AST.TNamed "SQLiteHeader" []

tPages :: Daedalus.AST.Type
tPages = Daedalus.AST.TList (Daedalus.AST.TNamed "Page" [])

