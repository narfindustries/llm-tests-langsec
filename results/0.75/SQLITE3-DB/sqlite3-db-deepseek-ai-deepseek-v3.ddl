SQLiteDB = {
  header: Header,
  pages: Page[header.db_size_in_pages]
};

Header = {
  magic: "SQLite format 3\000",
  page_size: U16BE,
  write_format: U8,
  read_format: U8,
  reserved_bytes: U8,
  max_embedded_payload_fraction: U8,
  min_embedded_payload_fraction: U8,
  leaf_payload_fraction: U8,
  file_change_counter: U32BE,
  db_size_in_pages: U32BE,
  first_freelist_trunk_page: U32BE,
  number_of_freelist_pages: U32BE,
  schema_cookie: U32BE,
  schema_format_number: U32BE,
  default_page_cache_size: U32BE,
  autovacuum_root: U32BE,
  text_encoding: U32BE,
  user_version: U32BE,
  incremental_vacuum_mode: U32BE,
  application_id: U32BE,
  reserved_expansion: U8[20],
  version_valid_for: U32BE,
  sqlite_version_number: U32BE
};

Page = {
  type: U8,
  first_freeblock: U16BE,
  number_of_cells: U16BE,
  start_of_cell_content_area: U16BE,
  fragmented_free_bytes: U8,
  rightmost_pointer: U32BE,
  cells: Cell[number_of_cells]
};

Cell = {
  payload_size: VarInt,
  row_id: VarInt,
  payload: U8[payload_size]
};

VarInt = {
  value: U8,
  if (value & 0x80) {
    next: VarInt
  }
};