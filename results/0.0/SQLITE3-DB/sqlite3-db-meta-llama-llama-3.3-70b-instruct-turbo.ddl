seq sqlite3_db {
  magic: uint32 = 0x53514c69,
  page_size: uint16,
  write_version: uint8,
  read_version: uint8,
  reserved: uint8 = 0,
  max_embedded_payload_fraction: uint8 = 64,
  min_embedded_payload_fraction: uint8 = 32,
  leaf_payload_fraction: uint8 = 32,
  seq page_headers {
    page_type: uint8,
    first_freeblock: uint16,
    cell_offset: uint16,
    num_cells: uint16,
    fragmented_free_bytes: uint8,
    switch (page_type) {
      0: seq btree_internal_node {
        left_child_page: uint32,
        right_child_page: uint32,
        key: varint
      },
      1: seq btree_internal_node {
        left_child_page: uint32,
        right_child_page: uint32,
        key: varint
      },
      2: seq btree_leaf_node {
        payload: varint
      },
      3: seq overflow_page {
        next_overflow_page: uint32,
        data: varint
      },
      4: seq index_btree_internal_node {
        left_child_page: uint32,
        right_child_page: uint32,
        key: varint
      },
      5: seq table_btree_leaf_node {
        payload: varint
      }
    }
  },
  seq master_journal {
    magic: uint32 = 0xd9d505f9,
    version: uint32 = 1,
    page_count: uint32,
    sector_size: uint32,
    page_size: uint32,
    reserved: uint64 = 0
  },
  seq wal {
    magic: uint32 = 0x02130d28,
    file_format_version: uint32 = 3007000,
    page_size: uint32,
    checksum_initial_value: uint32,
    salt1: uint32,
    salt2: uint32,
    reserved: uint64 = 0,
    seq frames {
      page_number: uint32,
      commit: uint8,
      reserved: uint24 = 0,
      salt1: uint32,
      salt2: uint32,
      checksum: uint32,
      data: varint
    }
  }
}