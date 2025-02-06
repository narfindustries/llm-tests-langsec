SQLiteDB = struct {
    header: Header;
    pages: Page[header.database_size];
};

Header = struct {
    header_string: bytes[16] == "SQLite format 3\0";
    page_size: uint16;
    write_version: uint8;
    read_version: uint8;
    reserved_space: uint8;
    max_embedded_payload: uint8;
    min_embedded_payload: uint8;
    leaf_payload_fraction: uint8;
    file_change_counter: uint32;
    database_size: uint32;
    first_freelist_page: uint32;
    freelist_page_count: uint32;
    schema_cookie: uint32;
    schema_format: uint32;
    default_cache_size: uint32;
    autovacuum_top_root: uint32;
    text_encoding: uint32;
    user_version: uint32;
    incremental_vacuum: uint32;
    application_id: uint32;
    reserved: bytes[20];
    version_valid_for: uint32;
    sqlite_version: uint32;
};

Page = struct {
    page_type: uint8;
    first_freeblock: uint16;
    cell_count: uint16;
    cell_content_offset: uint16;
    fragmented_bytes: uint8;
    right_child_pointer: uint32;
    cells: Cell[cell_count];
};

Cell = struct {
    row_id: varint;
    payload_size: varint;
    payload: bytes[payload_size];
    overflow_page_number: uint32 if payload_size > max_embedded_payload;
};

varint = struct {
    byte: uint8;
    next: varint if byte & 0x80;
};

FreelistTrunkPage = struct {
    next_trunk_page: uint32;
    leaf_page_count: uint32;
    leaf_page_numbers: uint32[leaf_page_count];
};

FreelistLeafPage = struct {
    next_leaf_page: uint32;
};

OverflowPage = struct {
    next_overflow_page: uint32;
    data: bytes[page_size - 4];
};

BTreeInteriorPage = Page & struct {
    page_type == 0x05;
};

BTreeLeafPage = Page & struct {
    page_type == 0x0D;
};

IndexInteriorPage = Page & struct {
    page_type == 0x02;
};

IndexLeafPage = Page & struct {
    page_type == 0x0A;
};

FreelistPage = Page & struct {
    page_type == 0x01 || page_type == 0x00;
    trunk: FreelistTrunkPage if page_type == 0x01;
    leaf: FreelistLeafPage if page_type == 0x00;
};

WALHeader = struct {
    magic: bytes[4] == "\x37\x7F\x06\x82";
    version: uint32;
    page_size: uint32;
    checkpoint_sequence_number: uint32;
    salt: uint32[2];
    checksum: uint32[2];
};

WALFrame = struct {
    page_number: uint32;
    page_data: bytes[page_size];
    salt: uint32[2];
    checksum: uint32[2];
};

WAL = struct {
    header: WALHeader;
    frames: WALFrame[];
};