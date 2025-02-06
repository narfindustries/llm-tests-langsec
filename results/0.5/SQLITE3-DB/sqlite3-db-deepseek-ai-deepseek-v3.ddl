SQLiteDB = struct {
    header: Header,
    pages: Page[header.database_size]
};

Header = struct {
    magic: string(16) == "SQLite format 3\0",
    page_size: uint16,
    write_version: uint8,
    read_version: uint8,
    reserved_space: uint8,
    max_payload: uint8,
    min_payload: uint8,
    leaf_payload: uint8,
    file_change_counter: uint32,
    database_size: uint32,
    first_freelist_page: uint32,
    freelist_page_count: uint32,
    schema_cookie: uint32,
    schema_format: uint32,
    default_cache_size: uint32,
    autovacuum_top_root: uint32,
    text_encoding: uint32,
    user_version: uint32,
    incremental_vacuum: uint32,
    application_id: uint32,
    reserved: bytes(20) == b"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
    version_valid_for: uint32,
    sqlite_version: uint32
};

Page = union {
    btree_page: BTreePage,
    freelist_trunk_page: FreelistTrunkPage,
    freelist_leaf_page: FreelistLeafPage,
    overflow_page: OverflowPage
};

BTreePage = struct {
    header: BTreePageHeader,
    cell_pointers: uint16[header.cell_count],
    cells: Cell[header.cell_count]
};

BTreePageHeader = struct {
    flags: uint8,
    first_free_block: uint16,
    cell_count: uint16,
    cell_content_offset: uint16,
    fragmented_bytes: uint8
};

Cell = union {
    leaf_cell: LeafCell,
    interior_cell: InteriorCell
};

LeafCell = struct {
    payload_length: varint,
    row_id: varint,
    payload: bytes(payload_length.value)
};

InteriorCell = struct {
    left_child_pointer: uint32,
    key: varint
};

FreelistTrunkPage = struct {
    next_trunk_page: uint32,
    number_of_leaves: uint32,
    leaf_page_numbers: uint32[number_of_leaves]
};

FreelistLeafPage = struct {
    next_leaf_page: uint32
};

OverflowPage = struct {
    next_overflow_page: uint32,
    payload: bytes(header.page_size - 4)
};

varint = struct {
    value: uint64,
    size: uint8
};