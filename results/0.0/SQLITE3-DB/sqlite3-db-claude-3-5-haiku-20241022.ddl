def sqlite3_file {
    magic: string(16),
    page_size: uint16 where (value >= 512 && value <= 65536 && (value & (value - 1)) == 0),
    write_version: uint8 where (value == 1 || value == 2),
    read_version: uint8 where (value == 1 || value == 2),
    reserved_space: uint8,
    max_embedded_payload_fraction: uint8,
    min_embedded_payload_fraction: uint8,
    leaf_payload_fraction: uint8,
    file_change_counter: uint32,
    database_size_pages: uint32,
    first_freelist_page: uint32,
    total_freelist_pages: uint32,
    schema_cookie: uint32,
    schema_format: uint32 where (value >= 1 && value <= 4),
    default_encoding: uint32 where (value >= 1 && value <= 3),
    user_version: uint32,
    incremental_vacuum_mode: uint32,
    application_id: uint32,
    reserved: list(uint8, 76)
}

def varint {
    value: uint64
}

def cell {
    payload_size: varint,
    row_id: varint,
    payload: list(uint8, payload_size.value)
}

def page {
    page_type: uint8 where (value == 2 || value == 5 || value == 10 || value == 13),
    first_freeblock: uint16,
    cell_count: uint16,
    cell_content_start: uint16,
    fragmented_free_bytes: uint8,
    cells: list(cell, cell_count)
}

def database {
    header: sqlite3_file,
    pages: list(page)
}