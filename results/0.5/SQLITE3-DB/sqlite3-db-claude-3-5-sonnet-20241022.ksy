meta:
  id: sqlite3
  file-extension: db
  endian: be

seq:
  - id: header
    type: database_header
  - id: pages
    type: page
    repeat: expr
    repeat-expr: header.database_size

types:
  database_header:
    seq:
      - id: magic
        contents: [0x53, 0x51, 0x4C, 0x69, 0x74, 0x65, 0x20, 0x66, 0x6F, 0x72, 0x6D, 0x61, 0x74, 0x20, 0x33, 0x00]
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
      - id: largest_root_btree
        type: u4
      - id: text_encoding
        type: u4
      - id: user_version
        type: u4
      - id: incremental_vacuum
        type: u4
      - id: application_id
        type: u4
      - id: reserved
        size: 20
      - id: version_valid_for
        type: u4
      - id: sqlite_version
        type: u4

  page:
    seq:
      - id: page_type
        type: u1
      - id: body
        type:
          switch-on: page_type
          cases:
            0x02: btree_page_interior_index
            0x05: btree_page_interior_table
            0x0a: btree_page_leaf_index
            0x0d: btree_page_leaf_table
            0x00: overflow_or_freelist_page

  btree_page_header:
    seq:
      - id: first_freeblock
        type: u2
      - id: cell_count
        type: u2
      - id: cell_content_offset
        type: u2
      - id: fragmented_free_bytes
        type: u1

  btree_page_interior_index:
    seq:
      - id: header
        type: btree_page_header
      - id: rightmost_pointer
        type: u4
      - id: cells
        type: btree_interior_cell_index
        repeat: expr
        repeat-expr: header.cell_count

  btree_page_interior_table:
    seq:
      - id: header
        type: btree_page_header
      - id: rightmost_pointer
        type: u4
      - id: cells
        type: btree_interior_cell_table
        repeat: expr
        repeat-expr: header.cell_count

  btree_page_leaf_index:
    seq:
      - id: header
        type: btree_page_header
      - id: cells
        type: btree_leaf_cell_index
        repeat: expr
        repeat-expr: header.cell_count

  btree_page_leaf_table:
    seq:
      - id: header
        type: btree_page_header
      - id: cells
        type: btree_leaf_cell_table
        repeat: expr
        repeat-expr: header.cell_count

  btree_interior_cell_index:
    seq:
      - id: left_child_page
        type: u4
      - id: len_payload
        type: u4
      - id: payload
        size: len_payload

  btree_interior_cell_table:
    seq:
      - id: left_child_page
        type: u4
      - id: row_id
        type: u4

  btree_leaf_cell_index:
    seq:
      - id: len_payload
        type: u4
      - id: payload
        size: len_payload

  btree_leaf_cell_table:
    seq:
      - id: len_payload
        type: u4
      - id: row_id
        type: u4
      - id: payload
        size: len_payload

  overflow_or_freelist_page:
    seq:
      - id: next_trunk_page
        type: u4
      - id: num_leaf_pages
        type: u4
      - id: leaf_pages
        type: u4
        repeat: expr
        repeat-expr: num_leaf_pages