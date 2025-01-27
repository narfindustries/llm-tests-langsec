domain sqlite3_db {
  import "sqlite3.ddl";

  type file_format {
    sequence {
      header: sqlite3_header;
      pages: *sqlite3_page;
    }
  }

  type sqlite3_header {
    uint32 magic;
    uint32 page_size;
    uint32 write_version;
    uint32 read_version;
    uint32 reserved_space;
    uint32 max_payload_fraction;
    uint32 min_payload_fraction;
    uint32 leaf_payload_fraction;
    uint32 file_change_counter;
    uint32 size_in_pages;
    uint32 first_freelist_page;
    uint32 number_of_freelist_pages;
    uint32 schema_cookie;
    uint32 schema_format_number;
    uint32 default_page_cache_size;
    uint32 largest_root_btree_page;
    uint32 text_encoding;
    uint32 user_version;
    uint32 incremental_vacuum_mode;
    uint32 application_id;
    bytes reserved(20);
  }

  type sqlite3_page {
    choice {
      case btree_page {
        sqlite3_btree_page page;
      }
      case freelist_page {
        sqlite3_freelist_page page;
      }
      case overflow_page {
        sqlite3_overflow_page page;
      }
    }
  }

  type sqlite3_btree_page {
    uint8 page_type;
    uint8 first_freeblock;
    uint16 cell_count;
    uint16 cell_offset;
    uint16 fragment_offset;
    uint8 num_fragments;
    bytes reserved(2);
    sequence {
      cell: *sqlite3_cell;
    }
  }

  type sqlite3_cell {
    uint16 payload_length;
    uint16 header_length;
    uint8 payload_type;
    bytes payload;
  }

  type sqlite3_freelist_page {
    uint32 freelist_trunk_page;
    sequence {
      freelist_page: *uint32;
    }
  }

  type sqlite3_overflow_page {
    uint32 next_overflow_page;
    bytes data;
  }
}