meta:
  id: sqlite3_db
  title: SQLite Database File
  application: SQLite
  file-extension: db
  endian: le
  license: CC0-1.0
doc: |
  SQLite database file format, used to store data in a structured format
  in a single file, used by SQLite, a lightweight database management system.

seq:
  - id: header
    type: file_header

  - id: pages
    type: page
    repeat: expr
    repeat-expr: header.num_pages

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
      - id: increment_vacuum_mode
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
      - id: data
        size: _root.header.page_size