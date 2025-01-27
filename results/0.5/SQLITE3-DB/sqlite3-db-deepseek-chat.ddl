-- Daedalus specification for SQLite3 database format
-- Improved and fixed based on the error message

struct SQLite3DB {
    magic: MagicNumber;
    pageSize: U16;
    fileFormatWriteVersion: U8;
    fileFormatReadVersion: U8;
    reservedSpace: U8;
    maxEmbeddedPayloadFraction: U8;
    minEmbeddedPayloadFraction: U8;
    leafPayloadFraction: U8;
    fileChangeCounter: U32;
    databaseSizeInPages: U32;
    firstFreelistPage: U32;
    numberOfFreelistPages: U32;
    schemaCookie: U32;
    schemaFormatNumber: U32;
    defaultPageCacheSize: U32;
    largestRootBtreePage: U32;
    textEncoding: U32;
    userVersion: U32;
    incrementalVacuumMode: U32;
    applicationId: U32;
    reserved: U32[20];
    versionValidFor: U32;
    sqliteVersionNumber: U32;
}

struct MagicNumber {
    value: U8[16] = [0x53, 0x51, 0x4C, 0x69, 0x74, 0x65, 0x20, 0x66, 0x6F, 0x72, 0x6D, 0x61, 0x74, 0x20, 0x33, 0x00];
}

-- Define the main database file structure
struct SQLite3File {
    header: SQLite3DB;
    pages: Page[databaseSizeInPages];
}

-- Define a page structure
struct Page {
    pageType: U8;
    data: U8[pageSize - 1];
}

-- Define the types used in the specification
type U8 = uint8;
type U16 = uint16;
type U32 = uint32;