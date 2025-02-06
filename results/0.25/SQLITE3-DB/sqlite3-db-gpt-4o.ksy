meta:
  id: sqlite3_db
  title: SQLite3 Database File
  file-extension: sqlite3
  endian: be

seq:
  - id: header
    type: header

types:
  header:
    seq:
      - id: header_string
        type: str
        size: 16
        encoding: ASCII
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
      - id: database_size_in_pages
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
      - id: largest_root_btree_page_number
        type: u4
      - id: text_encoding
        type: u4
      - id: user_version
        type: u4
      - id: incremental_vacuum_mode
        type: u4
      - id: application_id
        type: u4
      - id: reserved_for_expansion
        size: 20
      - id: version_valid_for_number
        type: u4
      - id: sqlite_version_number
        type: u4