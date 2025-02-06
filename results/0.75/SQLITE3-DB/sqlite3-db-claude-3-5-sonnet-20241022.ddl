def varint = {
    first: u8;
    let msb = (first band 0x80) != 0;
    let value = first band 0x7f;
    if msb {
        rest: u8[8];
        final: u8;
    }
}

def database_header = {
    magic: u8[16];
    page_size: u16;
    file_format_write_version: u8;
    file_format_read_version: u8;
    reserved_space: u8;
    max_payload_fraction: u8;
    min_payload_fraction: u8;
    leaf_payload_fraction: u8;
    file_change_counter: u32;
    database_size: u32;
    first_freelist_trunk: u32;
    freelist_pages: u32;
    schema_cookie: u32;
    schema_format: u32;
    default_page_cache: u32;
    largest_root_btree: u32;
    text_encoding: u32;
    user_version: u32;
    incremental_vacuum: u32;
    application_id: u32;
    reserved: u8[20];
    version_valid: u32;
    sqlite_version: u32;
}

def btree_page_header = {
    page_type: u8;
    first_freeblock: u16;
    cell_count: u16;
    cell_content_offset: u16;
    fragmented_free_bytes: u8;
    if (page_type == 2 or page_type == 5) {
        right_child: u32;
    }
}

def cell_pointer_array = {
    pointers: u16[cell_count];
}

def table_leaf_cell = {
    payload_size: varint;
    row_id: varint;
    payload: u8[payload_size];
}

def table_interior_cell = {
    left_child: u32;
    row_id: varint;
}

def index_leaf_cell = {
    payload_size: varint;
    payload: u8[payload_size];
}

def index_interior_cell = {
    left_child: u32;
    payload_size: varint;
    payload: u8[payload_size];
}

def cell = {
    if (page_type == 13) {
        content: table_leaf_cell;
    } else if (page_type == 5) {
        content: table_interior_cell;
    } else if (page_type == 10) {
        content: index_leaf_cell;
    } else if (page_type == 2) {
        content: index_interior_cell;
    }
}

def page = {
    if ($$ == 0) {
        header: database_header;
    } else {
        btree_header: btree_page_header;
        cell_pointers: cell_pointer_array;
        cells: cell[cell_count];
    }
}

def sqlite3_database = {
    pages: page[];
}

entry sqlite3_database