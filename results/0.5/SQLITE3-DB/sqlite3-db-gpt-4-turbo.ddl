module SQLite3;

import std.base;

struct Header {
    u8[16] magic = "SQLite format 3\0";
    u16 page_size;
    u8 write_version;
    u8 read_version;
    u8 reserved_space;
    u8 max_payload_frac;
    u8 min_payload_frac;
    u8 leaf_payload_frac;
    u32 file_change_counter;
    u32 database_size;
    u32 first_freelist_page;
    u32 num_freelist_pages;
    u32 schema_cookie;
    u32 schema_format;
    u32 default_cache_size;
    u32 largest_root_page;
    u32 text_encoding;
    u32 user_version;
    u32 incremental_vacuum;
    u32 application_id;
    u8[20] reserved;
    u32 version_valid_for;
    u32 sqlite_version;
}

struct BTreePageHeader {
    u8 page_type;
    u16 first_freeblock;
    u16 num_cells;
    u16 cell_content_offset;
    u8 num_fragmented_free_bytes;
    u32 right_most_pointer when (page_type == 2 || page_type == 5);
}

struct CellPointer {
    u16 pointer;
}

struct BTreeCell {
    varint payload_size;
    varint rowid when (parent(BTreePage).header.page_type == 13 || parent(BTreePage).header.page_type == 5);
    u32 left_child_page when (parent(BTreePage).header.page_type == 2 || parent(BTreePage).header.page_type == 5);
    u8[payload_size] payload;
}

struct OverflowPage {
    u32 next_overflow_page;
    std.base.bytes payload_data;
}

struct BTreePage {
    BTreePageHeader header;
    CellPointer[cell_pointers] cell_pointers;
    BTreeCell[cells] cells;
    OverflowPage[] overflow_pages;
}

struct DatabaseFile {
    Header header;
    BTreePage[pages] pages;
}

root DatabaseFile sqlite3_db;