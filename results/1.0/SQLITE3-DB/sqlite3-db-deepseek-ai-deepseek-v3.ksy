meta:
  id: sqlite3
  file-extension: sqlite3
  endian: be
seq:
  - id: magic
    contents: "SQLite format 3\0"
  - id: page_size
    type: u2
  - id: file_format_write_version
    type: u1
  - id: file_format_read_version
    type: u1
  - id: reserved_space
    type: u1
  - id: max_embedded_payload_fraction
    type: u1
  - id: min_embedded_payload_fraction
    type: u1
  - id: leaf_payload_fraction
    type: u1
  - id: file_change_counter
    type: u4
  - id: db_size_in_pages
    type: u4
  - id: first_freelist_trunk_page
    type: u4
  - id: total_freelist_pages
    type: u4
  - id: schema_cookie
    type: u4
  - id: schema_format_number
    type: u4
  - id: default_page_cache_size
    type: u4
  - id: largest_root_btree_page
    type: u4
  - id: text_encoding
    type: u4
  - id: user_version
    type: s4
  - id: incremental_vacuum_mode
    type: u4
  - id: application_id
    type: s4
  - id: reserved_for_expansion
    type: u4
  - id: version_valid_for_number
    type: u4
  - id: sqlite_version_number
    type: u4
types:
  btree_page:
    seq:
      - id: page_type
        type: u1
      - id: first_freeblock_offset
        type: u2
      - id: number_of_cells
        type: u2
      - id: offset_to_cell_content_area
        type: u2
      - id: fragmented_free_bytes
        type: u1
  freelist_page:
    seq:
      - id: next_freelist_trunk_page
        type: u4
      - id: number_of_free_pages
        type: u4
  pointer_map_page:
    seq:
      - id: page_type
        type: u1
      - id: parent_page_number
        type: u4
  wal_header:
    seq:
      - id: magic_number
        type: u4
      - id: file_format_version
        type: u4
      - id: page_size
        type: u4
      - id: checkpoint_sequence_number
        type: u4
      - id: salt_1
        type: u4
      - id: salt_2
        type: u4
      - id: checksum_1
        type: u4
      - id: checksum_2
        type: u4