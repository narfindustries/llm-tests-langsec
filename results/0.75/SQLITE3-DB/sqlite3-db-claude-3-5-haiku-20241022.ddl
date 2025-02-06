type sqlite3_db = {
    magic_header: ascii(length = 16),
    page_size: u16 where 512 <= value <= 65536,
    write_format: u8,
    read_format: u8,
    reserved_bytes: u8,
    max_payload_fraction: u8,
    min_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_page_count: u32,
    first_freelist_page: u32,
    total_freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u8,
    default_encoding: enum {
        UTF8 = 1,
        UTF16_LE = 2,
        UTF16_BE = 3
    } : u8,
    user_version: u16,
    incremental_vacuum_mode: u8,
    
    page_types: enum {
        INDEX_INTERIOR = 2,
        TABLE_INTERIOR = 5,
        INDEX_LEAF = 10,
        TABLE_LEAF = 13
    } : u8,
    
    record: {
        header_length: var_int,
        serial_types: [serial_type],
        column_data: [column_value]
    },
    
    serial_type: enum {
        NULL = 0,
        INT8 = 1,
        INT16 = 2,
        INT24 = 3,
        INT32 = 4,
        INT48 = 5,
        INT64 = 6,
        FLOAT = 7,
        ZERO = 8,
        ONE = 9,
        BLOB_TEXT = 10..255
    } : u8,
    
    var_int: fn() -> u64,
    column_value: fn(type: serial_type) -> any
}