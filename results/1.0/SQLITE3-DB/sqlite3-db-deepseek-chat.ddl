struct SQLite3DB {
    header: SQLite3Header;
    pages: Page[];
}

struct SQLite3Header {
    magic: MagicNumber;
    page_size: U16;
    file_format_write_version: U8;
    file_format_read_version: U8;
    reserved_space: U8;
    max_embedded_payload_fraction: U8;
    min_embedded_payload_fraction: U8;
    leaf_payload_fraction: U8;
    file_change_counter: U32;
    database_size_in_pages: U32;
    first_freelist_page: U32;
    number_of_freelist_pages: U32;
    schema_cookie: U32;
    schema_format_number: U32;
    default_page_cache_size: U32;
    largest_root_b_tree_page: U32;
    text_encoding: U32;
    user_version: U32;
    incremental_vacuum_mode: U32;
    application_id: U32;
    version_valid_for: U32;
    sqlite_version_number: U32;
}

struct MagicNumber {
    value: U8[16];
}

struct Page {
    page_type: U8;
    data: U8[page_size - 1];
}

struct U8 {
    value: byte;
}

struct U16 {
    value: byte[2];
}

struct U32 {
    value: byte[4];
}