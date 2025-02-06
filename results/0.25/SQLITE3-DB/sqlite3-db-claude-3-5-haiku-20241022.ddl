def sqlite3_db = {
  magic_header: bytes(length = 16) where magic_header == b"SQLite format 3\0",
  page_size: u16 where page_size >= 512 && page_size <= 65536 && (page_size & (page_size - 1)) == 0,
  write_version: u8 where write_version == 1 || write_version == 2,
  read_version: u8 where read_version == 1 || read_version == 2,
  reserved_space: u8,
  max_embedded_payload_fraction: u8,
  min_embedded_payload_fraction: u8,
  leaf_payload_fraction: u8,
  file_change_counter: u32,
  database_size_pages: u32,
  first_freelist_trunk_page: u32,
  total_freelist_pages: u32,
  schema_cookie: u32,
  schema_format_number: u32 where schema_format_number >= 1 && schema_format_number <= 4,
  default_encoding: u32 where default_encoding >= 1 && default_encoding <= 3,
  user_version: u32,
  incremental_vacuum_mode: u32,
  application_id: u32,
  reserved_bytes: bytes(length = 20),
  
  page_type: enum {
    leaf_table_b_tree = 0x0D,
    interior_table_b_tree = 0x05,
    leaf_index_b_tree = 0x0A,
    interior_index_b_tree = 0x02
  },
  
  varint: {
    bytes: u8[],
    value: u64
  },
  
  cell_structure: {
    payload_size: varint,
    row_id: varint,
    payload: bytes(length = payload_size.value)
  },
  
  b_tree_page: {
    page_header: {
      page_type: page_type,
      first_freeblock: u16,
      cell_count: u16,
      cell_content_start: u16,
      fragmented_free_bytes: u8
    },
    cell_pointers: u16[page_header.cell_count],
    cells: cell_structure[page_header.cell_count]
  },
  
  schema_table: {
    type: string,
    name: string,
    table_name: string,
    root_page: u32,
    sql_statement: string
  }
}