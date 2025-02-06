def SQLITE3_MAGIC = "SQLite format 3\000"

def BTREE_INTERIOR_INDEX = 0x02
def BTREE_INTERIOR_TABLE = 0x05
def BTREE_LEAF_INDEX = 0x0a
def BTREE_LEAF_TABLE = 0x0d
def OVERFLOW_PAGE = 0x00
def FREELIST_PAGE = 0x00

struct DatabaseHeader {
    magic: SQLITE3_MAGIC;
    page_size: u16;
    file_format_write_version: u8;
    file_format_read_version: u8;
    reserved_space: u8;
    max_payload_fraction: u8;
    min_payload_fraction: u8;
    leaf_payload_fraction: u8;
    file_change_counter: u32;
    db_size_pages: u32;
    first_freelist_trunk_page: u32;
    freelist_pages_count: u32;
    schema_cookie: u32;
    schema_format: u32;
    default_page_cache_size: u32;
    largest_root_btree_page: u32;
    text_encoding: u32;
    user_version: u32;
    incremental_vacuum: u32;
    application_id: u32;
    reserved: u8[20];
    version_valid_for: u32;
    sqlite_version: u32;
}

struct BTreePageHeader {
    page_type: u8;
    first_freeblock: u16;
    cell_count: u16;
    cell_content_offset: u16;
    fragmented_free_bytes: u8;
    right_child: if(page_type == BTREE_INTERIOR_INDEX || page_type == BTREE_INTERIOR_TABLE) u32;
}

struct Varint {
    while ((current_byte & 0x80) != 0) {
        bytes: u8;
    }
    final_byte: u8;
}

struct TableBTreeInteriorCell {
    page_number: u32;
    key: Varint;
}

struct TableBTreeLeafCell {
    payload_size: Varint;
    row_id: Varint;
    payload: u8[payload_size];
}

struct IndexBTreeInteriorCell {
    page_number: u32;
    payload: u8[];
}

struct IndexBTreeLeafCell {
    payload: u8[];
}

struct RecordHeader {
    header_size: Varint;
    serial_types: Varint[];
}

struct FreelistTrunkPage {
    next_trunk_page: u32;
    leaf_page_count: u32;
    leaf_pages: u32[leaf_page_count];
}

struct PointerMapEntry {
    type: u8;
}

struct Page {
    header: BTreePageHeader;
    cells: if(header.page_type == BTREE_INTERIOR_TABLE) TableBTreeInteriorCell[header.cell_count]
           else if(header.page_type == BTREE_LEAF_TABLE) TableBTreeLeafCell[header.cell_count]
           else if(header.page_type == BTREE_INTERIOR_INDEX) IndexBTreeInteriorCell[header.cell_count]
           else if(header.page_type == BTREE_LEAF_INDEX) IndexBTreeLeafCell[header.cell_count]
           else if(header.page_type == FREELIST_PAGE) FreelistTrunkPage;
}

struct SQLite3DB {
    header: DatabaseHeader;
    pages: Page[];
}

def Main = SQLite3DB