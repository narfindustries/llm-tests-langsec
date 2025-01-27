meta:
  id: sqlite3_db
  file-extension: sqlite3
  endian: le
  title: SQLite3 Database File
  application: SQLite

seq:
  - id: header
    type: header

types:
  header:
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
      - id: max_embedded_payload_frac
        type: u1
      - id: min_embedded_payload_frac
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
      - id: schema_format_number
        type: u4
      - id: default_page_cache_size
        type: u4
      - id: largest_btree_root_page
        type: u4
      - id: text_encoding
        type: u4
      - id: user_version
        type: u4
      - id: is_incremental_vacuum
        type: u4
      - id: application_id
        type: u4
      - id: reserved
        size: 20
      - id: version_valid_for
        type: u4
      - id: sqlite_version_number
        type: u4