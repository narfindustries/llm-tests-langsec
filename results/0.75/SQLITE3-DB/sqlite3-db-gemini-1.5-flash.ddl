module SQLite3 where

import Daedalus.AST

data SqliteHeader = SqliteHeader
  { magic :: ByteString -- 16 bytes "SQLite format 3\000"
  , pageSize :: Word16
  , writeVersion :: Word16
  , readVersion :: Word16
  , reserved1 :: ByteString -- 10 bytes
  , maxPageCount :: Word32
  , textEncoding :: Word32
  , userVersion :: Word32
  , dataVersion :: Word32
  , pageCount :: Word32
  , checksumFlag :: Word16
  , freePageCount :: Word16
  , schemaVersion :: Word32
  , stmtJrnlSize :: Word32
  , stmtJrnlPageCount :: Word32
  , sequenceCount :: Word32
  , reserved2 :: ByteString -- 2 bytes
  }

data PageHeader = PageHeader
  { pageNumber :: Word32
  , pageType :: Word8
  , freeByteCount :: Word8
  , checksum :: Word16
  , rightMostChildPageNumber :: Maybe Word32
  , cellCount :: Word16
  }

data CellHeader = CellHeader
  { cellSize :: Word32
  , payloadType :: Word8
  }

data CellPayload = CellPayload { payload :: ByteString }

data Cell = Cell { header :: CellHeader, payload :: CellPayload }

data SqlitePage = SqlitePage
  { header :: PageHeader
  , cells :: [Cell]
  }

sqliteDB :: Daedalus.AST.Type
sqliteDB = record [ ("header", SqliteHeader), ("pages", many SqlitePage) ]
