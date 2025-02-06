SQLITE3_DB :: struct {
    header: SQLITE3_Header;
    pages: SQLITE3_Page[header.database_size_in_pages];
};

SQLITE3_Header :: struct {
    header_string: bytes[16] == "SQLite format 3\000";
    page_size: uint16;
    file_format_write_version: uint8;
    file_format_read_version: uint8;
    reserved_space: uint8;
    maximum_embedded_payload_fraction: uint8;
    minimum_embedded_payload_fraction: uint8;
    leaf_payload_fraction: uint8;
    file_change_counter: uint32;
    database_size_in_pages: uint32;
    first_freelist_trunk_page: uint32;
    number_of_freelist_pages: uint32;
    schema_cookie: uint32;
    schema_format_number: uint32;
    default_page_cache_size: uint32;
    largest_b_tree_page_number: uint32;
    text_encoding: uint32;
    user_version: uint32;
    incremental_vacuum_mode: uint32;
    application_id: uint32;
    reserved_for_expansion: bytes[20] == "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
    version_valid_for: uint32;
    sqlite_version_number: uint32;
};

SQLITE3_Page :: union {
    case page_type: uint8 {
        0x02 => b_tree_interior_page: B_Tree_Interior_Page;
        0x0A => b_tree_leaf_page: B_Tree_Leaf_Page;
        0x05 => freelist_trunk_page: Freelist_Trunk_Page;
        0x0D => freelist_leaf_page: Freelist_Leaf_Page;
        0x06 => payload_overflow_page: Payload_Overflow_Page;
        0x07 => pointer_map_page: Pointer_Map_Page;
    };
};

B_Tree_Interior_Page :: struct {
    page_type: uint8 == 0x02;
    first_freeblock_offset: uint16;
    number_of_cells: uint16;
    cell_content_area_start: uint16;
    fragmented_free_bytes: uint8;
    rightmost_pointer: uint32;
    cells: B_Tree_Interior_Cell[number_of_cells];
};

B_Tree_Leaf_Page :: struct {
    page_type: uint8 == 0x0A;
    first_freeblock_offset: uint16;
    number_of_cells: uint16;
    cell_content_area_start: uint16;
    fragmented_free_bytes: uint8;
    cells: B_Tree_Leaf_Cell[number_of_cells];
};

Freelist_Trunk_Page :: struct {
    page_type: uint8 == 0x05;
    next_freelist_trunk_page: uint32;
    number_of_leaf_pages: uint32;
    leaf_page_numbers: uint32[number_of_leaf_pages];
};

Freelist_Leaf_Page :: struct {
    page_type: uint8 == 0x0D;
    next_freelist_leaf_page: uint32;
};

Payload_Overflow_Page :: struct {
    page_type: uint8 == 0x06;
    overflow_data: bytes[header.page_size - 4];
    next_overflow_page: uint32;
};

Pointer_Map_Page :: struct {
    page_type: uint8 == 0x07;
    pointer_map_entries: Pointer_Map_Entry[(header.page_size - 1) / 5];
};

Pointer_Map_Entry :: struct {
    page_type: uint8;
    parent_page_number: uint32;
};

B_Tree_Interior_Cell :: struct {
    left_child_page_number: uint32;
    key: varint;
};

B_Tree_Leaf_Cell :: struct {
    payload_size: varint;
    row_id: varint;
    payload: bytes[payload_size];
};

varint :: struct {
    value: uint64;
    size: uint8;
};