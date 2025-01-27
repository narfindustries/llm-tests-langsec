module SQLITE3_DB {

  import DAEDALUS::Core::*;

  struct Sqlite3Header {
    magic: array uint8 16;        // SQLite format 3\0
    page_size: uint16;            // 2 bytes
    write_version: uint8;         // 1 byte
    read_version: uint8;          // 1 byte
    reserved_space: uint8;        // 1 byte
    max_payload_frac: uint8;      // 1 byte
    min_payload_frac: uint8;      // 1 byte
    leaf_payload_frac: uint8;     // 1 byte
    change_counter: uint32;       // 4 bytes
    db_size_pages: uint32;        // 4 bytes
    first_freelist_page: uint32;  // 4 bytes
    num_freelist_pages: uint32;   // 4 bytes
    schema_cookie: uint32;        // 4 bytes
    schema_format: uint32;        // 4 bytes
    default_cache_size: uint32;   // 4 bytes
    largest_root_page: uint32;    // 4 bytes
    text_encoding: uint32;        // 4 bytes
    user_version: uint32;         // 4 bytes
    incremental_vacuum_mode: uint32; // 4 bytes
    application_id: uint32;         // 4 bytes
    version_valid_for: uint32;      // 4 bytes
    sqlite_version: uint32;         // 4 bytes
    reserved: array uint8 60;       // Reserved space for future expansion
  }

  validate Sqlite3Header::magic == [0x53, 0x51, 0x4C, 0x69, 0x74, 0x65, 0x20, 0x66, 0x6F, 0x72, 0x6D, 0x61, 0x74, 0x20, 0x33, 0x00];

  struct BTreePage {
    page_type: uint8;             // 1 byte: 2 for interior page, 10 for leaf page
    first_freeblock: uint16;      // 2 bytes
    num_cells: uint16;            // 2 bytes
    cell_content_area: uint16;    // 2 bytes
    fragmented_free_bytes: uint8; // 1 byte
    right_most_pointer: when (page_type == 2) uint32; // 4 bytes, only if interior page

    cells: vector<Cell> num_cells;
  }

  struct Cell {
    left_child_pointer: option uint32;
    payload_size: uint32;
    rowid: when (parent(BTreePage).page_type == 10) varint;
    payload: Payload;
  }

  struct Payload {
    data: switch (parent(Cell).payload_size) {
      case let s when s <= 127: array uint8 s;
      default: Blob;
    }
  }

  struct Blob {
    overflow_page_number: uint32;
    content: array uint8 (parent(Cell).payload_size - 4);
  }

  main := struct {
    header: Sqlite3Header;
    pages: vector<BTreePage> (header.db_size_pages - 1);
  };
}