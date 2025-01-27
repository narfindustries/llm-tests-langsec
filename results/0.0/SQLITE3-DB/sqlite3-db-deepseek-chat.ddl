-- SQLite3 Database File Format Specification in Daedalus
-- This specification defines the structure of an SQLite3 database file.

let HeaderString = "SQLite format 3\0"

struct SQLite3DB {
    header: HeaderString,
    page_size: be_u16,
    file_format_write_version: u8,
    file_format_read_version: u8,
    reserved_space: u8,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: be_u32,
    database_size_in_pages: be_u32,
    first_freelist_trunk_page: be_u32,
    total_freelist_pages: be_u32,
    schema_cookie: be_u32,
    schema_format_number: be_u32,
    default_page_cache_size: be_u32,
    largest_root_b_tree_page: be_u32,
    text_encoding: be_u32,
    user_version: be_u32,
    incremental_vacuum_mode: be_u32,
    application_id: be_u32,
    reserved: bytes[20],
    version_valid_for: be_u32,
    sqlite_version_number: be_u32
}

struct Page {
    page_type: u8,
    data: bytes[page_size - 1]
}

struct SQLite3DBFile {
    header: SQLite3DB,
    pages: Page[database_size_in_pages]
}