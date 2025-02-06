def sqlite3_file:
  type MagicHeader = bytes(16)
  type PageSize = u2 where constraint {
    condition: 512 <= value <= 65536
  }
  type Version = u1
  type ReservedSpace = u1
  type Fraction = u1
  type Counter = u4
  type PageNumber = u4
  type SchemaInfo = u4

  type EncodingType = enum u1 {
    UTF8 = 1,
    UTF16_LE = 2,
    UTF16_BE = 3
  }

  type FileHeader = struct {
    magic: MagicHeader,
    pageSize: PageSize,
    writeVersion: Version,
    readVersion: Version,
    reservedSpace: ReservedSpace,
    maxEmbeddedPayloadFraction: Fraction,
    minEmbeddedPayloadFraction: Fraction,
    leafPayloadFraction: Fraction,
    fileChangeCounter: Counter,
    databaseFileSize: PageNumber,
    firstFreelistTrunkPage: PageNumber,
    totalFreelistPages: PageNumber,
    schemaCookie: SchemaInfo,
    schemaFormat: SchemaInfo,
    defaultEncoding: EncodingType,
    userVersion: u2,
    incrementalVacuumMode: u4,
    applicationId: u4,
    reserved: bytes(20)
  }

  type PageType = enum u1 {
    INTERIOR_INDEX = 2,
    INTERIOR_TABLE = 5,
    LEAF_INDEX = 10,
    LEAF_TABLE = 13
  }

  type RecordHeader = struct {
    length: vlq,
    serialTypes: array(length, u1)
  }

  type Cell = struct {
    payloadSize: vlq,
    rowId: vlq,
    payload: bytes(payloadSize)
  }

  type Page = struct {
    type: PageType,
    firstFreeblock: u2,
    cellCount: u2,
    cellContentStart: u2,
    fragmentedFreeBytes: u1,
    cells: array(cellCount, Cell)
  }

  type SqliteFile = struct {
    header: FileHeader,
    pages: array(header.databaseFileSize / header.pageSize, Page)
  }