type HeaderFlags = struct {
  write_version: u8,
  read_version: u8
}

type DatabaseHeader = struct {
  magic: bytes(16),
  page_size: u16,
  flags: HeaderFlags,
  reserved_space: u8,
  max_payload_fraction: u8,
  min_payload_fraction: u8,
  leaf_payload_fraction: u8,
  file_change_counter: u32,
  db_size_pages: u32,
  first_freelist_trunk_page: u32,
  total_freelist_pages: u32,
  schema_cookie: u32,
  schema_format: u32,
  default_page_cache_size: u32,
  largest_root_btree: u32,
  text_encoding: u32,
  user_version: u32,
  vacuum_mode: u32,
  application_id: u32,
  reserved: bytes(20),
  version_valid_for: u32,
  sqlite_version: u32
}

type BTreePageHeader = struct {
  page_type: u8,
  first_freeblock: u16,
  cell_count: u16,
  cell_content_offset: u16,
  fragmented_free_bytes: u8,
  right_child: u32 if page_type == 0x02 || page_type == 0x05
}

type TableLeafCell = struct {
  payload_size: varint,
  rowid: varint,
  payload_header_size: varint,
  payload_header: varint[],
  payload: bytes(payload_size)
}

type TableInteriorCell = struct {
  left_child_page: u32,
  rowid: varint
}

type IndexLeafCell = struct {
  payload_size: varint,
  payload: bytes(payload_size)
}

type IndexInteriorCell = struct {
  left_child_page: u32,
  payload_size: varint,
  payload: bytes(payload_size)
}

type OverflowPage = struct {
  next_page: u32,
  data: bytes()
}

type FreelistPage = struct {
  next_trunk_page: u32,
  leaf_count: u32,
  page_numbers: u32[leaf_count]
}

type PtrmapEntry = struct {
  type: u8,
  page_number: u32
}

type Page = struct {
  header: BTreePageHeader,
  cells: TableLeafCell[header.cell_count] if header.page_type == 0x0D,
  cells: TableInteriorCell[header.cell_count] if header.page_type == 0x05,
  cells: IndexLeafCell[header.cell_count] if header.page_type == 0x0A,
  cells: IndexInteriorCell[header.cell_count] if header.page_type == 0x02,
  cells: FreelistPage if header.page_type == 0x00,
  cells: OverflowPage if header.page_type == 0x0B,
  cells: PtrmapEntry[] if header.page_type == 0x0F
}

type SQLiteDatabase = struct {
  header: DatabaseHeader,
  pages: Page[]
}

entry SQLiteDatabase