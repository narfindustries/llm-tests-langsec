meta:
  id: sqlite3_db
  file-extension: db
  endian: le

seq:
  - id: header
    type: header
  - id: pages
    type: page
    repeat: eos

types:
  header:
    seq:
      - id: magic
        contents: "SQLite format 3\0"
      - id: page_size
        type: u2
      - id: write_version
        type: u1
      - id: read_version
        type: u1
      - id: reserved_space
        type: u1
      - id: max_payload_fraction
        type: u1
      - id: min_payload_fraction
        type: u1
      - id: leaf_payload_fraction
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
      - id: schema_format
        type: u4
      - id: default_page_cache_size
        type: u4
      - id: largest_root_btree_page
        type: u4
      - id: text_encoding
        type: u4
      - id: user_version
        type: u4
      - id: vacuum_mode
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
      - id: cell_count
        type: u2
      - id: cell_content_area
        type: u2
      - id: fragmented_free_bytes
        type: u1
      - id: cells
        type: cell_pointer
        repeat: expr
        repeat-expr: cell_count
      - id: cell_content
        size-eos: true

  cell_pointer:
    seq:
      - id: offset
        type: u2