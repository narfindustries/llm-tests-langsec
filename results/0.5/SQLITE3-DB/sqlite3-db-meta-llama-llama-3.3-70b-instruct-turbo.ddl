domain sqlite3_db_meta {
  include "sqlite3_db.dl";

  type db_meta {
    bytes magic:4; // "SQLite"
    uint16 version;
    uint8 flags;
    uint8 encoding;
    uint16 pagesize;
    uint8 reserved_a;
    uint8 reserved_b;
    uint8 reserved_c;
    uint8 reserved_d;
    uint32 change_count;
    uint32 date_time;
    uint32 reserved_e;
    uint32 reserved_f;
    uint32 sql_reserved;
  }

  type page {
    uint8 type:1;
    uint16 page_size:15;
    uint32 page_count;
    uint32 free_page_count;
    uint32 schema_cookie;
    uint32 schema_format;
    uint32 page_size2;
    uint32 checksum;
  }

  type btree_page {
    page page_header;
    uint8 cell_count;
    bytes cell_pointers[];
    bytes cells[];
  }

  type cell {
    uint8 type;
    uint16 size;
    bytes data;
  }

  grammar {
    db_meta: magic version flags encoding pagesize reserved_a reserved_b reserved_c reserved_d change_count date_time reserved_e reserved_f sql_reserved;
    page: type page_size page_count free_page_count schema_cookie schema_format page_size2 checksum;
    btree_page: page_header cell_count cell_pointers cells;
    cell: type size data;
  }
}