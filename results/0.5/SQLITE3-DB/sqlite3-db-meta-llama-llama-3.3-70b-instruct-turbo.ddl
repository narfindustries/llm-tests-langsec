seq file_header {
  magic: 16 bytes = 0x53 0x51 0x4c 0x69 0x74 0x65 0x20 0x66 0x6f 0x72 0x6d 0x61 0x74 0x20 0x33 0x00
  page_size: uint16
  write_version: uint8
  read_version: uint8
  reserved_space: uint8
  max_embedded_payload_fraction: uint8
  min_embedded_payload_fraction: uint8
  leaf_payload_fraction: uint8
  file_change_counter: uint32
  num_pages: uint32
  first_freelist_trunk_page: uint32
  num_freelist_pages: uint32
  schema_cookie: uint32
  schema_format_number: uint32
  def_page_cache_size: uint32
  largest_root_btree_page: uint32
  text_encoding: uint32
  user_version: uint32
  incremental_vacuum_mode: uint32
  application_id: uint32
  reserved: 20 bytes
}

seq page_header {
  page_type: uint8
  first_freeblock: uint16
  num_cells: uint16
  cell_offset: uint16
  num_free_bytes: uint8
  right_child: uint32
  fragmented_free_bytes: uint8
}

seq cell {
  payload_length: uint32
  rowid: uint32
  payload: bytes -> payload_length
}

seq overflow_page {
  page_number: uint32
  data: bytes
}

seq pointer_map_page {
  page_number: uint32
  ptrmap_type: uint8
  ptrmap_page: bytes
}

seq btree_page {
  page_header: page_header
  cells: cell[]
  overflow_pages: overflow_page[]
}

seq page {
  page_header: page_header
  switch page_header.page_type {
    0: btree_page
    1: btree_page
    2: btree_page
    3: btree_page
    4: btree_page
    5: pointer_map_page
  }
}

seq sqlite_db {
  file_header: file_header
  pages: page[]
}