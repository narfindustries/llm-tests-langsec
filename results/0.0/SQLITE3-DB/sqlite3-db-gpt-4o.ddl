endianness big;

struct SQLite3DB {
    header: Header;
    pages: Page[header.database_size_in_pages];
}

struct Header {
    header_string: u8[16] == "SQLite format 3\0";
    page_size: u16;
    write_version: u8;
    read_version: u8;
    reserved_space: u8;
    max_embedded_payload_fraction: u8;
    min_embedded_payload_fraction: u8;
    leaf_payload_fraction: u8;
    file_change_counter: u32;
    database_size_in_pages: u32;
    first_freelist_trunk_page: u32;
    total_freelist_pages: u32;
    schema_cookie: u32;
    schema_format_number: u32;
    default_page_cache_size: u32;
    largest_root_btree_page_number: u32;
    text_encoding: u32;
    user_version: u32;
    incremental_vacuum_mode: u32;
    application_id: u32;
    reserved_for_expansion: u8[20];
    version_valid_for_number: u32;
    sqlite_version_number: u32;
}

struct Page {
    page_data: u8[header.page_size];
}