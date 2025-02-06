structure sqlite3_file_format {
  magic: string(16) = "SQLite format 3\000"
  page_size: uint16 >= 512 and <= 65536 and is_power_of_2
  write_version: uint8 = 1 or 2
  read_version: uint8 = 1 or 2
  reserved_space: uint8
  max_embedded_payload_fraction: uint8 = 64 or 32
  min_embedded_payload_fraction: uint8 = 32 or 64
  leaf_payload_fraction: uint8 = 32 or 64
  file_change_counter: uint32
  in_header_database_size: uint32
  first_freelist_trunk_page: uint32
  number_of_free_pages: uint32
  schema_cookie: uint32
  schema_format_number: uint32 >= 1 and <= 4
  default_page_cache_size: uint32
  largest_root_b_tree_page: uint32
  database_text_encoding: uint32 = 1 or 2 or 3
  user_version: uint32
  incremental_vacuum_mode: uint32 = 0 or 1
  application_id: uint32
}

structure page_header {
  page_type: uint8 = 0 or 1 or 2 or 3
  first_free_block: uint16
  number_of_cells: uint16
  start_of_cell_content_area: uint16
  fragmented_free_bytes: uint8
}

structure b_tree_page {
  page_type: uint8 = 1 or 2 or 3
  right_child_page: uint32
  number_of_keys: uint16
  keys: array(number_of_keys) of key
}

structure cell {
  payload_length: uint32
  header_length: uint8
  payload: bytes(payload_length)
  overflow_page: uint32
}

structure cell_header {
  type: uint8 = 0 or 1 or 2 or 3 or 4 or 5
  length: uint32
}

structure schema_format {
  format_number: uint8 >= 1 and <= 4
  schema: bytes
}

structure master_journal_header {
  magic: uint32 = 0xd926a1a5
  version: uint32
  master_journal_page: uint32
  master_journal_length: uint32
  master_journal_checksum: uint32
}

structure wal_header {
  magic: uint32 = 0x377f0682
  file_format_version: uint32 = 3007000
  page_size: uint32 >= 512 and <= 65536 and is_power_of_2
  checkpoint_sequence_number: uint32
  salt_1: uint32
  salt_2: uint32
  checksum_1: uint32
  checksum_2: uint32
}

structure key {
  key_value: bytes
}

root_type: sqlite3_file_format

predicate is_power_of_2(x: uint16): bool = (x & (x - 1)) == 0