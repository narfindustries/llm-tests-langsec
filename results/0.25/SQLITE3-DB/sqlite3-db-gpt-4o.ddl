endianness: big

struct SQLite3DB {
    header: Header;
    pages: Pages[header.database_size_in_pages];

    struct Header {
        header_string: string(16) == "SQLite format 3\0";
        page_size: uint16;
        file_format_write_version: uint8;
        file_format_read_version: uint8;
        reserved_space: uint8;
        max_embedded_payload_fraction: uint8;
        min_embedded_payload_fraction: uint8;
        leaf_payload_fraction: uint8;
        file_change_counter: uint32;
        database_size_in_pages: uint32;
        first_freelist_trunk_page: uint32;
        total_freelist_pages: uint32;
        schema_cookie: uint32;
        schema_format_number: uint32;
        default_page_cache_size: uint32;
        largest_root_btree_page_number: uint32;
        text_encoding: uint32;
        user_version: uint32;
        incremental_vacuum_mode: uint32;
        application_id: uint32;
        reserved_for_expansion: uint8[20];
        version_valid_for_number: uint32;
        sqlite_version_number: uint32;
    }

    struct Pages {
        data: uint8[header.page_size];
    }
}