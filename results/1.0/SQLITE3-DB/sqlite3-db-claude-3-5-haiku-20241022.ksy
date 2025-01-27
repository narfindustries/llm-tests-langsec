meta:
  id: sqlite3_db
  file-extension: db
  endian: le
seq:
  - id: header
    type: header
  - id: pages
    type: page
    repeat: until
    repeat-until: _io.is_eof
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
  page:
    seq:
      - id: page_type
        type: u1
      - id: first_freeblock
        type: u2
      - id: cell_count
        type: u2
      - id: cell_content_start
        type: u2
      - id: fragmented_free_bytes
        type: u1
      - id: cells
        type: cell
        repeat: expr
        repeat-expr: cell_count
  cell:
    seq:
      - id: payload_size
        type: u4
      - id: row_id
        type: u4
      - id: payload
        size: payload_size