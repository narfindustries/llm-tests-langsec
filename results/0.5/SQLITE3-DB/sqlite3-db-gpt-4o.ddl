module sqlite3_db_gpt_4o

-- SQLite Database File Format
-- Reference: https://www.sqlite.org/fileformat.html

pragma byteorder = big

struct SQLite3DB
{
    header: Header
    pages: Page[header.page_count]
}

struct Header
{
    magic: u32 -- 0x53514c69 'SQLite format 3\0'

    page_size: u16
    reserved_space: u8
    max_embedded_payload_frac: u8
    min_embedded_payload_frac: u8
    leaf_payload_frac: u8
    file_change_counter: u32
    database_size: u32
    first_freelist_trunk_page: u32
    total_freelist_pages: u32
    schema_cookie: u32
    schema_format_number: u32
    default_page_cache_size: u32
    largest_btree_root_page: u32
    text_encoding: u32
    user_version: u32
    incremental_vacuum_mode: u32
    application_id: u32
    reserved: u8[20]
    version_valid_for: u32
    sqlite_version_number: u32

    page_count: u32 = database_size / page_size
}

struct Page
{
    type: u8
    data: u8[header.page_size - 1]
}