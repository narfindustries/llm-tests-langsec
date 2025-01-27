meta:
  id: sqlite3_db
  file-extension: sqlite
  endian: le
  application: SQLite

seq:
  - id: header
    type: file_header

  - id: page
    type: page
    repeat: eos

types:
  file_header:
    seq:
      - id: signature
        contents: "SQLite format 3\0"
      - id: page_size
        type: u2
        if: page_size != 1
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
      - id: change_counter
        type: u4
      - id: db_size
        type: u4
      - id: freelist_first_trunk_page
        type: u4
      - id: freelist_page_count
        type: u4
      - id: schema_cookie
        type: u4
      - id: schema_format
        type: u4
      - id: page_cache_size
        type: u4
      - id: largest_root_page_num
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
      - id: sqlite_version_num
        type: u4

  page:
    seq:
      - id: page_type
        type: u1
      - id: first_freeblock
        type: u2
      - id: cell_count
        type: u2
      - id: cell_content_area
        type: u2
      - id: fragmented_free_bytes
        type: u1
      - id: payload
        size: _io.size - 8