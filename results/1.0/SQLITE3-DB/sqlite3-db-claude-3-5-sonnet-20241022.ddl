def DatabaseHeader:
    magic_header: u8[16]
    page_size: u16
    write_version: u8
    read_version: u8
    reserved_space: u8
    maximum_fraction: u8
    minimum_fraction: u8
    leaf_fraction: u8
    file_change_counter: u32
    database_size_pages: u32
    first_freelist_page: u32
    freelist_pages_count: u32
    schema_cookie: u32
    schema_format: u32
    page_cache_size: u32
    largest_root_btree: u32
    text_encoding: u32
    user_version: u32
    incremental_vacuum: u32
    application_id: u32
    reserved: u8[20]
    version_valid: u32
    sqlite_version: u32

def PageHeader:
    page_type: u8
    first_freeblock: u16
    cells_count: u16
    cell_content_offset: u16
    fragmented_free_bytes: u8
    if page_type == 2 or page_type == 5:
        rightmost_pointer: u32

def Varint:
    value: u64

def CellPointerArray:
    cell_pointers: u16[]

def TableLeafCell:
    payload_size: Varint
    rowid: Varint
    payload: RecordFormat

def TableInteriorCell:
    left_child_page: u32
    rowid: Varint

def IndexLeafCell:
    payload_size: Varint
    payload: RecordFormat

def IndexInteriorCell:
    left_child_page: u32
    payload_size: Varint
    payload: RecordFormat

def RecordFormat:
    header_length: Varint
    serial_types: Varint[]
    values: DataValue[]

def DataValue:
    type: u64
    if type == 0:
        null
    elif type == 1:
        value: i8
    elif type == 2:
        value: i16
    elif type == 3:
        value: i24
    elif type == 4:
        value: i32
    elif type == 5:
        value: i48
    elif type == 6:
        value: i64
    elif type == 7:
        value: f64
    elif type >= 12 and type % 2 == 0:
        blob: u8[(type - 12) / 2]
    elif type >= 13 and type % 2 == 1:
        text: string[(type - 13) / 2]

def FreelistPage:
    next_freelist_page: u32
    pages_count: u32
    page_numbers: u32[pages_count]

def Page:
    header: PageHeader
    cell_pointers: CellPointerArray
    if header.page_type == 13:
        cells: TableLeafCell[header.cells_count]
    elif header.page_type == 5:
        cells: TableInteriorCell[header.cells_count]
    elif header.page_type == 10:
        cells: IndexLeafCell[header.cells_count]
    elif header.page_type == 2:
        cells: IndexInteriorCell[header.cells_count]

def SQLiteDatabase:
    header: DatabaseHeader
    pages: Page[]

main SQLiteDatabase