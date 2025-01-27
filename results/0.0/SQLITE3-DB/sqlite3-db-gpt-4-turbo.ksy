meta:
  id: sqlite3_db
  title: SQLite Database File
  application: SQLite
  file-extension: sqlite3
  endian: le
  license: CC0-1.0
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
    type: page
    repeat: eos

types:
  file_header:
    seq:
      - id: magic
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
      - id: num_pages
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
      - id: num_cells
        type: u2
      - id: cell_content_area
        type: u2
      - id: fragmented_free_bytes
        type: u1
      - id: right_most_pointer
        type: u4
      - id: cells
        type: cell
        repeat: expr
        repeat-expr: num_cells

  cell:
    seq:
      - id: left_child_page
        type: u4
      - id: rowid
        type: u4
      - id: payload
        size: 0 # Placeholder, actual implementation needed based on the database schema