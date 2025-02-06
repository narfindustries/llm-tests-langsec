module SQLite3DB {
  enum Encoding : uint8 {
    UTF8 = 1,
    UTF16LE = 2,
    UTF16BE = 3
  }

  enum PageType : uint8 {
    IndexInterior = 2,
    TableInterior = 5,
    IndexLeaf = 10,
    TableLeaf = 13
  }

  enum SerialType : uint8 {
    Null = 0,
    Int8 = 1,
    Int16 = 2,
    Int24 = 3,
    Int32 = 4,
    Int48 = 5,
    Int64 = 6,
    Float64 = 7,
    Zero = 8,
    One = 9,
    Reserved1 = 10,
    Reserved2 = 11,
    BlobVar = 12,
    TextVar = 13
  }

  struct SQLiteHeader {
    magic: string(16),
    page_size: uint16,
    write_version: uint8,
    read_version: uint8,
    reserved_space: uint8,
    max_embedded_payload: uint8,
    min_embedded_payload: uint8,
    leaf_payload_fraction: uint8,
    file_change_counter: uint32,
    database_pages: uint32,
    first_freelist_page: uint32,
    total_freelist_pages: uint32,
    schema_cookie: uint32,
    schema_format: uint32,
    default_encoding: Encoding,
    user_version: uint32,
    incremental_vacuum: uint32
  }

  struct RecordHeader {
    header_length: uint64,
    serial_types: list(SerialType)
  }

  struct SQLitePage {
    header: SQLiteHeader,
    pages: list(PageType)
  }

  function parse_sqlite(input: stream): SQLitePage
}