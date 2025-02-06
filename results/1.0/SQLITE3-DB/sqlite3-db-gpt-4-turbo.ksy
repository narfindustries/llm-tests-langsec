meta:
  id: sqlite3
  title: SQLite Database File Format 3
  application: SQLite
  file-extension: db
  endian: le
  license: Public Domain
doc: |
  SQLite is a C library that implements an SQL database engine. A large subset of SQL92 is supported.  This file format represents SQLite database files.

seq:
  - id: header
    type: file_header

  - id: pages
    type: page
    repeat: expr
    repeat-expr: header.db_size

types:
  file_header:
    seq:
      - id: signature
        contents: "SQLite format 3\x00"
      - id: page_size
        type: u2
      - id: write_version
        type: u1
      - id: read_version
        type: u1
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
      - id: db_size
        type: u4
      - id: first_freelist_trunk_page
        type: u4
      - id: num_freelist_pages
        type: u4
      - id: schema_cookie
        type: u4
      - id: schema_format
        type: u4
      - id: default_cache_size
        type: s4
      - id: largest_root_page
        type: u4
      - id: text_encoding
        enum: encoding
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
      - id: page_header
        type: b_tree_page_header

      - id: cells
        type: cell
        repeat: expr
        repeat-expr: page_header.num_cells

      - id: cell_content_area
        size: page_header.cell_content_area_sz

  b_tree_page_header:
    seq:
      - id: page_type
        enum: page_type
        type: u1
      - id: first_freeblock
        type: u2
      - id: num_cells
        type: u2
      - id: cell_content_area_start
        type: u2
      - id: num_fragmented_free_bytes
        type: u1
      - id: right_most_pointer
        type: u4
        if: page_type != page_type::leaf_table and page_type != page_type::leaf_index

  cell:
    seq:
      - id: left_child_page
        type: u4
        if: _parent.page_header.page_type == page_type::interior_index or _parent.page_header.page_type == page_type::interior_table
      - id: payload_size
        type: vlq_base128_le
      - id: rowid
        type: vlq_base128_le
        if: _parent.page_header.page_type == page_type::leaf_table
      - id: payload
        size: payload_size
      - id: overflow_page_number
        type: u4
        if: payload_size > _parent.page_header.cell_content_area_start - payload_size

enums:
  encoding:
    1: utf_8
    2: utf_16le
    3: utf_16be

  page_type:
    2: interior_index
    5: interior_table
    10: leaf_index
    13: leaf_table

types:
  vlq_base128_le:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: _ & 0x80 == 0
    instances:
      value:
        value: >
          _.reduce(bytes, (acc, cur) => (acc << 7) | (cur & 0x7F), 0)

instances:
  cell_content_area_sz:
    doc: Calculate size of the cell content area.
    value: '((_parent.header.page_size - 1) - _parent.header.reserved_space) - page_header.cell_content_area_start'