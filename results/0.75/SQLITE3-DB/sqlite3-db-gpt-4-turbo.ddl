module SQLite3DB {
  import std::uint8;
  import std::uint16;
  import std::uint32;
  import std::uint64;
  import std::varint;
  import std::array;

  struct DatabaseHeader {
    magic: array[uint8, 16];
    pageSize: uint16;
    writeVersion: uint8;
    readVersion: uint8;
    reservedSpace: uint8;
    maxPayloadFrac: uint8;
    minPayloadFrac: uint8;
    leafPayloadFrac: uint8;
    changeCounter: uint32;
    databaseSize: uint32;
    firstFreelistTrunkPage: uint32;
    totalFreelistPages: uint32;
    schemaCookie: uint32;
    schemaFormat: uint32;
    defaultCacheSize: uint32;
    largestRootPage: uint32;
    textEncoding: uint32;
    userVersion: uint32;
    incrementVersion: uint32;
  }

  struct Page {
    header: PageHeader;
    content: dynamic[PageContent];
  }

  struct PageHeader {
    pageType: uint8;
  }

  struct BTreePageHeader {
    numFreeblocks: uint16;
    numCells: uint16;
    startOfContent: uint16;
    fragmentedFreeBytes: uint8;
    rightMostPointer: conditional[uint32, this.parent.header.pageType == 2 || this.parent.header.pageType == 5];
  }

  struct BTreeInteriorPage {
    header: BTreePageHeader;
    cellPointers: array[uint16, this.header.numCells];
  }

  struct BTreeLeafPage {
    header: BTreePageHeader;
    cells: array[Cell, this.header.numCells];
  }

  struct Cell {
    leftChildPage: conditional[uint32, this.parent.header.pageType == 2 || this.parent.header.pageType == 5];
    payloadSize: varint;
    rowid: conditional[varint, this.parent.header.pageType == 10 || this.parent.header.pageType == 13];
    payload: array[uint8, this.payloadSize];
  }

  struct PageContent {
    case this.header.pageType {
      2 => BTreeInteriorPage;
      5 => BTreeInteriorPage;
      10 => BTreeLeafPage;
      13 => BTreeLeafPage;
    }
  }

  struct SQLiteFile {
    header: DatabaseHeader;
    pages: array[Page, this.header.databaseSize];
  }
}