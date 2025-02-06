enum uint8 PageType {
    InteriorIndexBTree = 2;
    LeafIndexBTree = 10;
    InteriorTableBTree = 5;
    LeafTableBTree = 13;
}

enum uint32 be TextEncoding {
    UTF8 = 1;
    UTF16le = 2;
    UTF16be = 3;
}

struct SQLite3Header {
    header_string : string(16);
    page_size : uint16 be;
    write_version : uint8;
    read_version : uint8;
    reserved_space : uint8;
    max_embedded_payload_frac : uint8;
    min_embedded_payload_frac : uint8;
    leaf_payload_frac : uint8;
    file_change_counter : uint32 be;
    database_size : uint32 be;
    first_freelist_page : uint32 be;
    freelist_page_count : uint32 be;
    schema_cookie : uint32 be;
    schema_format_number : uint32 be;
    default_page_cache_size : uint32 be;
    large_file_flag : uint32 be;
    database_text_encoding : TextEncoding;
    user_version : uint32 be;
    incremental_vacuum_mode : uint32 be;
    application_id : uint32 be;
    reserved : bytes(20);
    version_valid_for : uint32 be;
    sqlite_version_number : uint32 be;
}

struct SQLite3DB {
    header : SQLite3Header;
    pages : Page[header.database_size];
}

struct Page {
    page_type : PageType;
    first_freeblock_offset : uint16 be;
    cell_content_offset : uint16 be;
    number_of_cells : uint16 be;
    right_most_pointer : uint32 be?;
}

struct BTreePage extends Page {
    cells : BTreeCell[number_of_cells];
}

struct BTreeCell {
    left_child_page : uint32 be?;
    data : bytes;
}

struct FreeListPage extends Page {
    next_freelist_trunk_page : uint32 be;
    freelist_leaf_page_count : uint32 be;
}