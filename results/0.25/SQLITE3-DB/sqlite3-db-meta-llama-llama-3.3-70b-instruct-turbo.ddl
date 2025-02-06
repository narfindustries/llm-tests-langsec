seq sqlite3_db_file {
  magic: string(16) = "SQLite format 3\000"
  page_size: uint16
  write_version: uint8
  read_version: uint8
  reserved_space: uint8
  max_embedded_payload_fraction: uint8
  min_embedded_payload_fraction: uint8
  leaf_payload_fraction: uint8
  file_change_counter: uint32
  size_of_database_in_pages: uint32
  first_freelist_trunk_page: uint32
  number_of_freelist_pages: uint32
  schema_cookie: uint32
  schema_format_number: uint8
  default_page_cache_size: uint32
  largest_root_btree_page: uint32
  text_encoding: uint8
  user_version: uint32
  incremental_vacuum_mode: uint8
  application_id: uint32
  reserved_for_expansion: bytes(20)
  version_valid_for: uint32
  sqlite_version_number: uint32
  pages: seq(page)
}

seq page {
  page_type: uint8
  first_freeblock: uint16
  cell_content_area: uint16
  number_of_cells: uint16
  start_of_content_area: uint16
  number_of_fragmented_free_bytes: uint8
  cells: seq(btree_cell)
}

seq btree_cell {
  length_of_payload: uint8
  header_length: uint8
  number_of_columns: uint8
  column_data: bytes(length_of_payload)
  header: btree_cell_header
}

seq btree_cell_header {
  varint: varint
  serial_type: uint8
  payload: bytes(serial_type)
}

seq varint {
  value: uint64
}

seq payload {
  type: uint8
  data: bytes
}

seq sqlite3_db {
  header: seq(sqlite3_db_file)
}

seq page_header {
  page_type: uint8
  first_freeblock: uint16
  cell_content_area: uint16
  number_of_cells: uint16
  start_of_content_area: uint16
  number_of_fragmented_free_bytes: uint8
}

seq btree_page {
  header: page_header
  cells: seq(btree_cell)
}

seq overflow_page {
  header: page_header
  data: bytes
}

seq freelist_page {
  header: page_header
  data: bytes
}

seq freelist_trunk {
  header: page_header
  data: bytes
}

seq sqlite3_page {
  type: uint8
  data: bytes
}

seq sqlite3_cell {
  payload: bytes
  header: btree_cell_header
}