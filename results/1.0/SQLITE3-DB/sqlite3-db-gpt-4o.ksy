meta:
  id: sqlite3_db
  file-extension: sqlite
  application: SQLite
  endian: le

seq:
  - id: header
    type: header

types:
  header:
    seq:
      - id: magic
        contents: 'SQLite format 3\0'
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
      - id: largest_root_b_tree_page_number
        type: u4
      - id: text_encoding
        type: u4
      - id: user_version
        type: u4
      - id: incremental_vacuum_mode
        type: u4
      - id: application_id
        type: u4
      # 20 bytes of unused space
      - id: unused_space
        size: 20
      - id: version_valid_for
        type: u4
      - id: sqlite_version_number
        type: u4