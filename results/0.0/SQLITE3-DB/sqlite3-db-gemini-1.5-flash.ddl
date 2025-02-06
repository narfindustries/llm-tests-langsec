data SqliteHeader = SqliteHeader
  { magic :: String 16
  , pageSize :: Word16
  , writeVersion :: Word16
  , readVersion :: Word16
  , reservedByte :: Word8
  , maxPageCount :: Word32
  , textEncoding :: Word32
  , userVersion :: Word32
  , incrementalVacuumMode :: Word32
  , applicationId :: Word64
  , versionValidForAllPages :: Word32
  , pageCount :: Word32
  , checksumFlag :: Word32
  , freePageCount :: Word32
  , schemaVersion :: Word32
  , schemaFormat :: Word32
  , defaultPageCacheSize :: Word32
  , largeFileSupport :: Word32
  , pageSizeExtension :: Word32
  , reservedBytes :: String 20
  }

data PageHeader = PageHeader
  { pageNumber :: Word32
  , pageType :: Word8
  , freeBlockCount :: Word8
  , cellCount :: Word16
  , firstFreeBlock :: Word16
  , cellPointer :: Word16
  , checksum :: Word32
  }

data CellHeader = CellHeader
  { headerSize :: Word16
  , rowId :: Word64
  , payloadSize :: Word16
  , offsetToPayload :: Word16
  }

data CellPayload = CellPayload
  { payload :: String
  }

data SqlitePage = SqlitePage
  { header :: PageHeader
  , cells :: [CellHeader]
  , payloads :: [CellPayload]
  }

data SqliteDatabase = SqliteDatabase
  { header :: SqliteHeader
  , pages :: [SqlitePage]
  }
