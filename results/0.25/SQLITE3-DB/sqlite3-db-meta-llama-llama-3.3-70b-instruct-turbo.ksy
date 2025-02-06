meta:
  id: sqlite3_db
  file-extension: db
  endian: le
  title: SQLite database file format

seq:
  - id: magic
    type: str
    size: 16
    encoding: ascii
  - id: file_format_version
    type: u2
  - id: page_size
    type: u2
  - id: read_write_version
    type: u1
  - id: reserved_space
    type: u1
  - id: max_embedded_payload_fraction
    type: u1
  - id: min_embedded_payload_fraction
    type: u1
  - id: leaf_pointer_map
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
  - id: schema_format_number
    type: u4
  - id: default_page_cache_size
    type: u4
  - id: largest_root_btree_page
    type: u4
  - id: text_encoding
    type: u4
  - id: user_version
    type: u4
  - id: incremental_vacuum_mode
    type: u4
  - id: application_id
    type: u4
  - id: pages
    type: page
    repeat: expr
    repeat-expr: database_size / page_size

types:
  page:
    seq:
      - id: page_type
        type: u1
      - id: first_freeblock
        type: u2
      - id: cell_offset_array
        type: u2
      - id: num_cells
        type: u2
      - id: start_of_content_area
        type: u2
      - id: cells
        type: cell
        repeat: expr
        repeat-expr: num_cells

  cell:
    seq:
      - id: payload_length
        type: uvarint
      - id: row_id
        type: uvarint
      - id: num_columns
        type: uvarint
      - id: column_data
        type: column
        repeat: expr
        repeat-expr: num_columns

  column:
    seq:
      - id: type
        type: u1
      - id: value
        type:
          switch-on: type
          cases:
            - 0: uvarint
            - 1: str
            - 2: bytes
            - 3: uvarint
            - 4: str
            - 5: bytes
            - 6: uvarint
            - 7: str
            - 8: bytes

  btree:
    seq:
      - id: page_number
        type: u4
      - id: right_child_page_number
        type: u4
      - id: cell_key
        type: cell

  uvarint:
    process: zlib
    zlib-windowbits: 0

instances:
  num_cells: num_cells
  database_size: database_size
  page_size: page_size

enums:
  page_type:
    0: leaf_index
    1: internal_index
    2: leaf_table
    3: internal_table
    4: overflow
    5: pointer_map

  text_encoding:
    0: utf8
    1: utf16le
    2: utf16be