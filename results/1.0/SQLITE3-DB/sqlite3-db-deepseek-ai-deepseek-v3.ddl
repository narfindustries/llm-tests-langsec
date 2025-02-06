SQLiteDB = {
    header: SQLiteHeader,
    pages: SQLitePage[]
};

SQLiteHeader = {
    magic: MagicString,
    pageSize: uint16,
    writeVersion: uint8,
    readVersion: uint8,
    reservedSpace: uint8,
    maxEmbeddedPayload: uint8,
    minEmbeddedPayload: uint8,
    leafPayload: uint8,
    fileChangeCounter: uint32,
    databaseSize: uint32,
    firstFreelistPage: uint32,
    freelistPages: uint32,
    schemaCookie: uint32,
    schemaFormat: uint32,
    defaultPageCache: uint32,
    largestRootPage: uint32,
    textEncoding: uint32,
    userVersion: uint32,
    incrementalVacuum: uint32,
    applicationID: uint32,
    reserved: uint8[20],
    versionValidFor: uint32,
    sqliteVersion: uint32
};

MagicString = uint8[16] where $ == "SQLite format 3\0";

SQLitePage = {
    pageType: uint8 where $.oneOf([0x02, 0x05, 0x0A, 0x0D]),
    freeblockOffset: uint16,
    cellCount: uint16,
    cellContentOffset: uint16,
    fragmentedBytes: uint8,
    rightChildPointer: uint32 if pageType.oneOf([0x02, 0x05]),
    cells: SQLiteCell[cellCount]
};

SQLiteCell = {
    payloadSize: Varint,
    rowID: Varint if pageType.oneOf([0x0A, 0x0D]),
    childPageNumber: Varint if pageType.oneOf([0x02, 0x05]),
    payload: uint8[payloadSize]
};

Varint = {
    bytes: uint8[] where $.length <= 9,
    value: uint64 = decodeVarint(bytes)
};

decodeVarint = (bytes: uint8[]): uint64 => {
    result = 0;
    shift = 0;
    for (b in bytes) {
        result |= (b & 0x7F) << shift;
        if ((b & 0x80) == 0) {
            break;
        }
        shift += 7;
    }
    return result;
};