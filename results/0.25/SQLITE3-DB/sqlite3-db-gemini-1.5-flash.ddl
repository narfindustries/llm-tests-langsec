data SQLite3Header = SQLite3Header {
  magic :: [Word8],
  pageSize :: Word32,
  writeVersion :: Word32,
  readOnly :: Word32,
  reservedSpace :: Word32,
  maxPageCount :: Word32,
  changeCount :: Word32,
  format :: Word32,
  textEncoding :: Word32,
  userVersion :: Word32,
  pageSize2 :: Word32,
  writeVersion2 :: Word32,
  reservedSpace2 :: Word32,
  unused :: [Word8]
}

data PageHeader = PageHeader {
  pageType :: Word8,
  pageNumber :: Word32,
  freeBlockCount :: Word16,
  cells :: [Cell]
}

data Cell = Cell {
  header :: CellHeader,
  data :: [Word8]
}

data CellHeader = CellHeader {
  cellType :: Word8,
  size :: Word32
}

data SQLite3Database = SQLite3Database {
  header :: SQLite3Header,
  pages :: [Page]
}

data Page = Page {
  header :: PageHeader,
  data :: [Word8]
}
