module SQLite3;

type uint8 = UInt8;
type uint16 = UInt16le;
type uint32 = UInt32le;
type uint64 = UInt64le;

struct DatabaseHeader {
    signature: bytes[16] = b"SQLite format 3\0";
    pageSize: uint16;
    writeFormat: uint8;
    readFormat: uint8;
    reservedSpace: uint8;
    maxEmbeddedPayloadFrac: uint8 = 64;
    minEmbeddedPayloadFrac: uint8 = 32;
    leafPayloadFrac: uint8 = 32;
    fileChangeCounter: uint32;
    databaseSize: uint32;
    firstFreelistTrunkPage: uint32;
    totalFreelistPages: uint32;
    schemaCookie: uint32;
    schemaFormatNumber: uint32;
    defaultPageCacheSize: uint32;
    largestRootPage: uint32;
    databaseTextEncoding: uint32;
    userVersion: uint32;
    isIncrementalVacuum: uint32;
    applicationId: uint32;
    reserved20: bytes[20];
    versionValidFor: uint32;
    sqliteVersionNumber: uint32;
}

struct PageHeader {
    pageType: uint8;
    firstFreeblock: uint16;
    numCells: uint16;
    cellContentOffset: uint16;
    numFragmentedFreeBytes: uint8;
}

struct Cell {
    leftChildPage: uint32; // Only for interior cells
    payload: bytes;
}
enum PageType {
    InteriorIndex = 0x02,
    InteriorTable = 0x05,
    LeafIndex = 0x0A,
    LeafTable = 0x0D
}

struct BTreePage {
    header: PageHeader;
    rightMostPointer: uint32;  // Only for interior cells
    cellPointers: uint16[];
    cells: Cell[];
}

struct OverflowPage {
    nextPage: uint32;
    content: bytes;
}

struct PointerMapPage {
    entries: PointerMapEntry[];
}

struct PointerMapEntry {
    pageType: uint8;
    parentPageNumber: uint32;
}

struct FreelistPage {
    nextFreelistPage: uint32;
    numFreeCells: uint16;
    freeCellPointers: uint16[];
}

struct Payload {
    size: uint64; // Varint
    data: bytes;
}

struct DatabaseFile {
    header: DatabaseHeader;
    pages: (BTreePage | OverflowPage | PointerMapPage | FreelistPage)[];
}