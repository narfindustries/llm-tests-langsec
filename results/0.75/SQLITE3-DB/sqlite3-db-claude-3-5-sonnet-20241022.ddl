module sqlite3_db_claude_3_5_sonnet_20241022

import stdlib::formats::*

def Main = {
    let header = SQLiteHeader()
    let pages = Many0(Page)
}

def SQLiteHeader = {
    $"SQLite format 3"
    magic_string: Uint8[16]
    page_size: UBigEndian(16)
    write_version: Uint8
    read_version: Uint8
    reserved_space: Uint8
    max_fraction: Uint8
    min_fraction: Uint8
    leaf_payload: Uint8
    file_change_counter: UBigEndian(32)
    database_size: UBigEndian(32)
    first_freelist: UBigEndian(32)
    total_freelist: UBigEndian(32)
    schema_cookie: UBigEndian(32)
    schema_format: UBigEndian(32)
    default_page_cache: UBigEndian(32)
    largest_root_btree: UBigEndian(32)
    text_encoding: UBigEndian(32)
    user_version: UBigEndian(32)
    incremental_vacuum: UBigEndian(32)
    application_id: UBigEndian(32)
    version_valid_for: UBigEndian(32)
    sqlite_version: UBigEndian(32)
}

def Page = {
    page_type: Uint8
    let content = Choose {
        1 => InteriorIndexBtreePage
        2 => InteriorTableBtreePage 
        5 => LeafIndexBtreePage
        10 => LeafTableBtreePage
        13 => FreelistTrunkPage
    }
}

def CellPointerArray = {
    num_cells: UBigEndian(16)
    cell_pointers: UBigEndian(16)[num_cells]
}

def InteriorIndexBtreePage = {
    pointers: CellPointerArray
    rightmost_pointer: UBigEndian(32)
    cells: Many0(InteriorIndexCell)
}

def InteriorTableBtreePage = {
    pointers: CellPointerArray
    rightmost_pointer: UBigEndian(32)
    cells: Many0(InteriorTableCell)
}

def LeafIndexBtreePage = {
    pointers: CellPointerArray
    cells: Many0(LeafIndexCell)
}

def LeafTableBtreePage = {
    pointers: CellPointerArray
    cells: Many0(LeafTableCell)
}

def FreelistTrunkPage = {
    next_trunk: UBigEndian(32)
    leaf_count: UBigEndian(32)
    leaf_pages: UBigEndian(32)[leaf_count]
}

def InteriorIndexCell = {
    left_child_page: UBigEndian(32)
    payload_length: VarInt
    payload: Bytes(payload_length)
}

def InteriorTableCell = {
    left_child_page: UBigEndian(32)
    row_id: VarInt
}

def LeafIndexCell = {
    payload_length: VarInt
    payload: Bytes(payload_length)
}

def LeafTableCell = {
    payload_length: VarInt
    row_id: VarInt
    payload: Bytes(payload_length)
}

def VarInt = {
    first_byte: Uint8
    Choose {
        first_byte < 0x80 => Value(first_byte)
        first_byte < 0xC0 => {
            second_byte: Uint8
            Value((first_byte - 0x80) << 7 | second_byte)
        }
        first_byte < 0xE0 => {
            remaining_bytes: Uint8[3]
            Value((first_byte - 0xC0) << 14 | remaining_bytes[0] << 7 | remaining_bytes[1])
        }
        _ => {
            remaining_bytes: Uint8[8]
            Value(first_byte << 56 | remaining_bytes[0] << 48 | remaining_bytes[1] << 40 |
                 remaining_bytes[2] << 32 | remaining_bytes[3] << 24 | remaining_bytes[4] << 16 |
                 remaining_bytes[5] << 8 | remaining_bytes[6])
        }
    }
}