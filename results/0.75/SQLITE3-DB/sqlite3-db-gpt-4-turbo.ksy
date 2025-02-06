meta:
  id: sqlite3_db
  title: SQLite Database File Format 3
  endian: le
  file-extension: db
doc: |
  SQLite3 database file format specification, covering the main database file structure, including the database header, pages, and b-tree structures.

seq:
  - id: header
    type: file_header

  - id: pages
    type: page
    repeat: expr
    repeat-expr: header.in_header_db_size - 1

types:
  file_header:
    seq:
      - id: signature
        contents: "SQLite format 3\000"
      - id: page_size
        type: u2
      - id: write_version
        type: u1
        enum: write_version
      - id: read_version
        type: u1
        enum: read_version
      - id: reserved_space
        type: u1
      - id: max_payload_frac
        type: u1
      - id: min_payload_frac
        type: u1
      - id: leaf_payload_frac
        type: u1
      - id: file_change_counter
        type: u4
      - id: in_header_db_size
        type: u4
      - id: first_freelist_trunk_page
        type: u4
      - id: num_freelist_pages
        type: u4
      - id: schema_cookie
        type: u4
      - id: schema_format
        type: u4
        enum: schema_format
      - id: default_cache_size
        type: u4
      - id: largest_root_b_tree_page
        type: u4
      - id: db_text_encoding
        type: u4
        enum: db_text_encoding
      - id: user_version
        type: u4
      - id: incremental_vacuum_mode
        type: u4
      - id: application_id
        type: u4
      - id: reserved_expansion
        size: 20
      - id: version_valid_for_number
        type: u4
      - id: sqlite_version_number
        type: u4

  page:
    seq:
      - id: page_type
        type: u1
        enum: page_type
      - id: first_freeblock
        type: u2
      - id: num_cells
        type: u2
      - id: cell_content_offset
        type: u2
      - id: num_fragmented_free_bytes
        type: u1
      - id: right_most_pointer
        type: u4
        if: page_type == page_type::interior_index or page_type == page_type::interior_table
      - id: cells
        type: cell
        repeat: expr
        repeat-expr: num_cells

  cell:
    seq:
      - id: left_child_page
        type: u4
        if: _parent.page_type == page_type::interior_table or _parent.page_type == page_type::interior_index
      - id: payload_size
        type: vlq_base128_le
      - id: row_id
        type: vlq_base128_le
        if: _parent.page_type == page_type::leaf_table
      - id: payload
        size: payload_size

enums:
  write_version:
    1: legacy
    2: wal

  read_version:
    1: legacy
    2: wal

  schema_format:
    1: format_1
    2: format_2
    3: format_3
    4: format_4

  db_text_encoding:
    1: utf_8
    2: utf_16le
    3: utf_16be

  page_type:
    2: interior_index
    5: interior_table
    10: leaf_index
    13: leaf_table
    255: overflow

types:
  vlq_base128_le:
    seq:
      - id: groups
        type: u1
        repeat: until
        repeat-until: _.groups & 0x80 == 0
    instances:
      value:
        value: |
          _.reduce((a, b) => a | ((b & 0x7F) << (7 * index)), 0)