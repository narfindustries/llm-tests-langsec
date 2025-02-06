module SQLite3DB;

import std.core;

enum FileFormatVersion {
  Legacy = 1,
  WAL = 2
}

enum TextEncoding {
  UTF8 = 1,
  UTF16le = 2,
  UTF16be = 3
}

struct DatabaseHeader {
  headerString : bytes[16];
  pageSize : uint16;
  writeVersion : FileFormatVersion;
  readVersion : FileFormatVersion;
  reservedSpace : uint8;
  maxPayloadFrac : uint8;
  minPayloadFrac : uint8;
  leafPayloadFrac : uint8;
  fileChangeCounter : uint32;
  databaseSize : uint32;
  firstFreelistPage : uint32;
  numFreelistPages : uint32;
  schemaCookie : uint32;
  schemaFormat : uint32;
  defaultPageCacheSize : uint32;
  largestRootBtreePage : uint32;
  textEncoding : TextEncoding;
  userVersion : uint32;
  incrementalVacuumMode : uint32;
  applicationId : uint32;
  reserved : bytes[20];
  versionValidFor : uint32;
  sqliteVersion : uint32;
}

enum PageType {
  InteriorIndex = 2,
  InteriorTable = 5,
  LeafIndex = 10,
  LeafTable = 13
}

struct BtreePageHeader {
  pageType : PageType;
  firstFreeblock : uint16;
  numCells : uint16;
  startOfContentArea : uint16;
  fragmentedFreeBytes : uint8;
  rightMostPointer : uint32 ? (pageType == PageType.InteriorIndex || pageType == PageType.InteriorTable);
}

struct CellPointer {
  offset : uint16;
}

struct BtreePage {
  header : BtreePageHeader;
  cellPointerArray : array<CellPointer>;
  // Additional fields for cell content would be defined here, depending on the schema and data type
}

struct FreelistPage {
  nextFreelistPage : uint32;
  numFreeblocks : uint32;
  // Assuming additional structure for freeblock management if needed
}

struct SQLite3File {
  header : DatabaseHeader;
  pages : array<variant<BtreePage, FreelistPage>>;
}

root SQLite3File;