let Main = Sqlite3Db

let Sqlite3Db = {
    header: Header
    pages: Page[]
}

let Header = {
    magic: "SQLite format 3\000"
    pageSize: U16
    fileFormatWriteVersion: U8
    fileFormatReadVersion: U8
    reservedSpace: U8
    maxEmbeddedPayloadFraction: U8
    minEmbeddedPayloadFraction: U8
    leafPayloadFraction: U8
    fileChangeCounter: U32
    databaseSizePages: U32
    firstFreelistTrunkPage: U32
    totalFreelistPages: U32
    schemaCookie: U32
    schemaFormat: U32
    defaultPageCacheSize: U32
    largestRootBtreePage: U32
    databaseTextEncoding: U32
    userVersion: U32
    incrementalVacuumMode: U32
    applicationId: U32
    reserved: U8[20]
    versionValidFor: U32
    sqliteVersionNumber: U32
}

let Page = {
    pageType: U8
    content: PageContent
}

let PageContent = {
    | InteriorIndexBtree  { pageType == 0x02 }
    | InteriorTableBtree  { pageType == 0x05 }
    | LeafIndexBtree      { pageType == 0x0A }
    | LeafTableBtree      { pageType == 0x0D }
    | FreePage            { pageType == 0x00 }
    | PointerMap          { pageType == 0x0F }
    | LockByte            { pageType == 0x01 }
}

let BtreePageHeader = {
    firstFreeblock: U16
    cellCount: U16
    cellContentOffset: U16
    fragmentedFreeBytes: U8
}

let InteriorBtreePage = {
    header: BtreePageHeader
    rightChild: U32
    cells: InteriorCell[header.cellCount]
}

let LeafBtreePage = {
    header: BtreePageHeader
    cells: LeafCell[header.cellCount]
}

let InteriorCell = {
    leftChild: U32
    rowId: Varint
}

let LeafCell = {
    payloadSize: Varint
    rowId: Varint
    payload: Record
    overflowPage: U32?
}

let FreePage = {
    nextFreePage: U32
    unused: U8[]
}

let PointerMap = {
    entries: PtrmapEntry[]
}

let PtrmapEntry = {
    entryType: U8
    pageNumber: U32
}

let LockByte = {
    lockBytes: U8[]
}

let Record = {
    headerLength: Varint
    serialTypes: Varint[headerLength]
    values: RecordValue[headerLength]
}

let RecordValue = {
    | Null                { serialType == 0 }
    | Int8(U8)           { serialType == 1 }
    | Int16(U16)         { serialType == 2 }
    | Int24(U24)         { serialType == 3 }
    | Int32(U32)         { serialType == 4 }
    | Int48(U48)         { serialType == 5 }
    | Int64(U64)         { serialType == 6 }
    | Float64(F64)       { serialType == 7 }
    | Zero(U8)           { serialType == 8 }
    | One(U8)            { serialType == 9 }
    | Blob(U8[])         { serialType >= 12 && serialType % 2 == 0 }
    | Text(Char[])       { serialType >= 13 && serialType % 2 == 1 }
}

let Varint = {
    bytes: U8[]          { byte & 0x80 != 0 }
    finalByte: U8
}