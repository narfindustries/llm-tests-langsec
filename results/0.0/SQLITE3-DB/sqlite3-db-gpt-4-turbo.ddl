module SQLITE3_DB {
  import DAEDALUS::Core;

  type SQLiteFile = struct {
    header: SQLiteHeader,
    body:   array Page { length = header.page_count }
  };

  type SQLiteHeader = struct {
    magic:         bytes { length = 16 },
    page_size:     uint16,
    file_format_write_version: uint8,
    file_format_read_version: uint8,
    reserved_space: uint8,
    max_payload_frac: uint8,
    min_payload_frac: uint8,
    leaf_payload_frac: uint8,
    file_change_counter: uint32,
    database_size_pages: uint32,
    first_freelist_trunk_page: uint32,
    total_freelist_pages: uint32,
    schema_cookie: uint32,
    schema_format: uint32,
    default_cache_size: uint32,
    largest_root_btree_page: uint32,
    text_encoding: uint32,
    user_version: uint32,
    incremental_vacuum_mode: uint32,
    application_id: uint32,
    reserved: bytes { length = 20 },
    version_valid_for: uint32,
    sqlite_version_number: uint32,
    page_count: uint32 computed { database_size_pages }
  };

  type Page = union {
    case header.page_type == 0x02 => IndexInteriorPage,
    case header.page_type == 0x05 => TableInteriorPage,
    case header.page_type == 0x0A => IndexLeafPage,
    case header.page_type == 0x0D => TableLeafPage
  } switch (uint8 page_type);

  type IndexInteriorPage = struct {
    header: PageHeader,
    cells: array Cell { length = header.num_cells }
  };

  type TableInteriorPage = struct {
    header: PageHeader,
    cells: array Cell { length = header.num_cells }
  };

  type IndexLeafPage = struct {
    header: PageHeader,
    cells: array Cell { length = header.num_cells }
  };

  type TableLeafPage = struct {
    header: PageHeader,
    cells: array Cell { length = header.num_cells }
  };

  type PageHeader = struct {
    page_type: uint8,
    first_freeblock: uint16,
    num_cells: uint16,
    cell_content_area: uint16,
    fragmented_free_bytes: uint8,
    right_most_pointer: uint32 optional { page_type == 0x02 || page_type == 0x05 }
  };

  type Cell = struct {
    left_child_pointer: uint32 optional { page_type == 0x02 || page_type == 0x05 },
    payload_size: uint16,
    rowid: uint64 optional { page_type == 0x0A || page_type == 0x0D },
    payload: bytes { length = payload_size }
  };
}