-- Daedalus specification for SQLite3 database format
-- Improved and fixed based on the error message

def SQLite3DB = block
    header: SQLiteHeader
    pages: ManyPages

def SQLiteHeader = block
    magic: magic "\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00"
    page_size: uint16
    write_version: uint8
    read_version: uint8
    reserved: uint8
    max_embedded_payload_fraction: uint8
    min_embedded_payload_fraction: uint8
    leaf_payload_fraction: uint8
    file_change_counter: uint32
    database_size_in_pages: uint32
    first_freelist_page: uint32
    number_of_freelist_pages: uint32
    schema_cookie: uint32
    schema_format: uint32
    default_page_cache_size: uint32
    largest_root_btree_page: uint32
    text_encoding: uint32
    user_version: uint32
    incremental_vacuum_mode: uint32
    application_id: uint32
    reserved_for_expansion: uint8[20]
    version_valid_for: uint32
    sqlite_version_number: uint32

def ManyPages = block
    pages: Page[database_size_in_pages]

def Page = block
    page_type: uint8
    data: switch page_type
        case 0x0D: BTreePage
        case 0x05: FreelistPage
        default: RawPage

def BTreePage = block
    flags: uint8
    cell_count: uint16
    cell_content_start: uint16
    fragmented_free_bytes: uint8
    right_most_pointer: uint32
    cells: BTreeCell[cell_count]

def BTreeCell = block
    payload_size: varint
    row_id: varint
    payload: bytes[payload_size]

def FreelistPage = block
    next_freelist_page: uint32
    freelist_trunk_pages: uint32[number_of_freelist_pages]

def RawPage = block
    raw_data: bytes[page_size - 1]

def varint = block
    byte: uint8
    value: switch byte & 0x80
        case 0x80: (byte & 0x7F) | (varint << 7)
        default: byte