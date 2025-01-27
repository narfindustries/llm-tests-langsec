meta:
  id: sqlite3_db
  file-extension: db
  endian: be

seq:
  - id: header
    type: header
  - id: pages
    type: page
    repeat: eos

types:
  header:
    seq:
      - id: magic
        contents: [0x53, 0x51, 0x4c, 0x69, 0x74, 0x65, 0x20, 0x66, 0x6f, 0x72, 0x6d, 0x61, 0x74, 0x20, 0x33, 0x00]
      - id: page_size
        type: u2
      - id: write_version
        type: u1
      - id: read_version
        type: u1
      - id: reserved_space
        type: u1
      - id: max_payload_fraction
        type: u1
      - id: min_payload_fraction
        type: u1
      - id: leaf_payload_fraction
        type: u1
      - id: file_change_counter
        type: u4
      - id: database_size
        type: u4
      - id: first_freelist_trunk_page
        type: u4
      - id: total_freelist_pages
        type: u4
      - id: schema_cookie
        type: u4
      - id: schema_format
        type: u4
      - id: default_page_cache_size
        type: u4
      - id: largest_root_btree_page
        type: u4
      - id: database_text_encoding
        type: u4
      - id: user_version
        type: u4
      - id: incremental_vacuum_mode
        type: u4
      - id: application_id
        type: u4
      - id: reserved
        size: 20
      - id: version_valid_for
        type: u4
      - id: sqlite_version_number
        type: u4

  page:
    seq:
      - id: page_type
        type: u1
      - id: first_freeblock
        type: u2
      - id: cell_count
        type: u2
      - id: cell_content_area
        type: u2
      - id: fragmented_free_bytes
        type: u1
      - id: right_most_pointer
        type: u4
        if: "page_type == 0x02 or page_type == 0x05"
      - id: cell_pointers
        type: u2
        repeat: expr
        repeat-expr: cell_count
      - id: content
        size-eos: true

    enums:
      page_types:
        0x02: interior_index
        0x05: interior_table
        0x0a: leaf_index
        0x0d: leaf_table
        0x00: freelist_trunk
        0x01: freelist_leaf