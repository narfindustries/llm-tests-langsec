domain sqlite3_db_meta {
  endian = little;

  type file_header_t {
    uint32(4) magic;
    uint32(1) page_size;
    uint32(1) write_version;
    uint32(1) read_version;
    uint32(1) reserved_SPACE;
    uint32(1) max_embedded_payload_fraction;
    uint32(1) min_embedded_payload_fraction;
    uint32(1) leaf_payload_fraction;
    uint32(4) file_change_counter;
    uint32(4) integrity_check;
  }

  type btree_page_header_t {
    uint8(1) type;
    uint8(1) first_freeblock_offset;
    uint16(2) num_cells;
    uint16(2) num_fragmented_free_bytes;
    uint32(4) right_child_page;
    uint32(4) num_keys;
  }

  type cell_t {
    uint32(4) payload_length;
    uint32(4) key_length;
    uint32(4) payload_offset;
    bytes(key_length) key;
    bytes(payload_length) payload;
  }

  type b_tree_page_t {
    file_header_t header;
    btree_page_header_t page_header;
    cell_t cells[];
  }

  bytes = b_tree_page_t;
}