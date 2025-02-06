module SQLite3 where

import Daedalus.Type.AST

data SQLiteHeader = SQLiteHeader
  { magic :: String 16
  , pageSize :: Word16
  , writeCount :: Word32
  , pageSizeExtension :: Word32
  , fileFormatExtension :: Word32
  , reservedBytes :: [Word8] 10
  }

data PageHeader = PageHeader
  { pageNumber :: Word32
  , pageType :: Word8
  , freeblockOffset :: Word16
  , numberOfCells :: Word16
  , rightChildPage :: Word32
  , cellPointers :: [Word32]
  }

data CellData = CellData {
    type_ :: Word8,
    payload :: Bytes
  }

data Cell = Cell
  { cellData :: CellData
  }

data SQLiteDatabase = SQLiteDatabase
  { header :: SQLiteHeader
  , pages :: [Page]
  }

data Page = Page
  { header :: PageHeader
  , cells :: [Cell]
  }

parseSQLiteHeader :: Parser SQLiteHeader
parseSQLiteHeader = do
  magic <- string "SQLite format 3"
  pageSize <- word16le
  writeCount <- word32le
  pageSizeExtension <- word32le
  fileFormatExtension <- word32le
  reservedBytes <- bytes 10
  return $ SQLiteHeader {..}

parsePageHeader :: Parser PageHeader
parsePageHeader = do
  pageNumber <- word32le
  pageType <- word8
  freeblockOffset <- word16le
  numberOfCells <- word16le
  rightChildPage <- word32le
  cellPointers <- count (fromIntegral . numberOfCells) (word32le)
  return $ PageHeader {..}

parseCellData :: Parser CellData
parseCellData = do
  type_ <- word8
  payload <- bytes
  return $ CellData {..}

parseCell :: Parser Cell
parseCell = do
  cellData <- parseCellData
  return $ Cell {..}

parsePage :: Parser Page
parsePage = do
  header <- parsePageHeader
  cells <- replicateM (fromIntegral $ numberOfCells header) parseCell
  return $ Page {..}

parseSQLiteDatabase :: Parser SQLiteDatabase
parseSQLiteDatabase = do
  header <- parseSQLiteHeader
  pages <- many parsePage
  return $ SQLiteDatabase {..}

main :: IO ()
main = return ()
