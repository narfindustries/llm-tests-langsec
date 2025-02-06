meta:
  id: sqlite3_db
  title: SQLite Database File Format 3
  application: SQLite
  file-extension: sqlite3
  endian: le
  license: PD

doc: |
  SQLite is a C library that provides a lightweight disk-based database that doesn’t require
  a separate server process and allows accessing the database using a nonstandard variant
  of the SQL query language. Some applications can use SQLite for internal data storage.
  It’s also possible to prototype an application using SQLite and then port the code to a
  larger database such as PostgreSQL or Oracle.

seq:
  - id: header
    type: file_header

  - id: pages
    type: btree_page
    repeat: expr
    repeat-expr: header.db_size_in_pages

types:
  file_header:
    seq:
      - id: header_string
        contents: "SQLite format 3\0"
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
      - id: db_size_in_pages
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
        type: u4
      - id: largest_root_page
        type: u4
      - id: db_text_encoding
        enum: text_encoding
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

  btree_page:
    seq:
      - id: page_type
        enum: page_types
        type: u1
      - id: first_freeblock
        type: u2
      - id: num_cells
        type: u2
      - id: cell_content_offset
        type: u2
      - id: num_fragmented_free_bytes
        type: u1
      - id: cells
        type: cell
        repeat: expr
        repeat-expr: num_cells
      - id: right_most_pointer
        type: u4
        if: page_type == page_types::interior_index or page_type == page_types::interior_table

  cell:
    seq:
      - id: left_child_page
        type: u4
        if: _parent.page_type == page_types::interior_index or _parent.page_type == page_types::interior_table
      - id: payload_size
        type: vlq_base128_le
      - id: rowid
        type: vlq_base128_le
        if: _parent.page_type == page_types::leaf_table
      - id: payload
        size: payload_size

enums:
  page_types:
    2: interior_index
    5: interior_table
    10: leaf_index
    13: leaf_table

  text_encoding:
    1: utf_8
    2: utf_16le
    3: utf_16be

types:
  vlq_base128_le:
    seq:
      - id: groups
        type: u1
        repeat: until
        repeat-until: _.last_group
    instances:
      value:
        value: |
          _.groups.map((group, i) => (group & 0x7F) << (7 * i)).reduce((a, b) => a + b)
      last_group:
        value: groups[-1] < 0x80