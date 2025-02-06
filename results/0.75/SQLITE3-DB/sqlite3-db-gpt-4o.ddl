struct SQLITE3_DB {
    header: SQLiteHeader,
    pages: Page[header.database_size_in_pages]
}

struct SQLiteHeader {
    header_string: bytes[16],  // Should be "SQLite format 3\0"
    page_size: u16be,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_payload_fraction: u8,
    min_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32be,
    database_size_in_pages: u32be,
    first_freelist_trunk_page: u32be,
    total_freelist_pages: u32be,
    schema_cookie: u32be,
    schema_format_number: u32be,
    default_page_cache_size: u32be,
    largest_btree_page_number: u32be,
    database_text_encoding: u32be,
    user_version: u32be,
    incremental_vacuum_mode: u32be,
    application_id: u32be,
    reserved_for_expansion: bytes[20],  // Should be zero
    version_valid_for_number: u32be,
    sqlite_version_number: u32be
}

struct Page {
    page_type: u8,
    body: PageBody(page_type)
}

union PageBody(page_type) {
    2 => InteriorIndexPage,
    5 => InteriorTablePage,
    10 => LeafIndexPage,
    13 => LeafTablePage,
    _ => RawPageContent
}

struct InteriorIndexPage {
    first_freeblock_offset: u16be,
    number_of_cells: u16be,
    start_of_cell_content_area: u16be,
    fragmented_free_bytes: u8,
    rightmost_pointer: u32be,
    cells: Cell[number_of_cells]
}

struct InteriorTablePage {
    first_freeblock_offset: u16be,
    number_of_cells: u16be,
    start_of_cell_content_area: u16be,
    fragmented_free_bytes: u8,
    rightmost_pointer: u32be,
    cells: Cell[number_of_cells]
}

struct LeafIndexPage {
    first_freeblock_offset: u16be,
    number_of_cells: u16be,
    start_of_cell_content_area: u16be,
    fragmented_free_bytes: u8,
    cells: Cell[number_of_cells]
}

struct LeafTablePage {
    first_freeblock_offset: u16be,
    number_of_cells: u16be,
    start_of_cell_content_area: u16be,
    fragmented_free_bytes: u8,
    cells: Cell[number_of_cells]
}

struct RawPageContent {
    content: bytes[variable]
}

struct Cell {
    // Cell structure can be defined with more precision based on specific usage
    content: bytes[variable]
}