meta:
  id: sqlite3
  file-extension: sqlite3
  endian: be
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
        contents: "SQLite format 3\000"
        size: 16
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
      - id: first_freelist_page
        type: u4
      - id: freelist_pages
        type: u4
      - id: schema_cookie
        type: u4
      - id: schema_format
        type: u4
      - id: cache_size
        type: u4
      - id: largest_root_page
        type: u4
      - id: text_encoding
        type: u4
      - id: user_version
        type: u4
      - id: incremental_vacuum
        type: u4
      - id: application_id
        type: u4
      - id: reserved
        size: 20
      - id: version_valid_for
        type: u4
      - id: sqlite_version
        type: u4
  page:
    seq:
      - id: page_type
        type: u1
      - id: first_free_block
        type: u2
      - id: cell_count
        type: u2
      - id: cell_content_start
        type: u2
      - id: fragmented_free_bytes
        type: u1
      - id: cell_pointers
        type: u2
        repeat: expr
        repeat-expr: cell_count
      - id: cell_content
        type: cell
        repeat: expr
        repeat-expr: cell_count
  cell:
    seq:
      - id: header_size
        type: u1
      - id: serial_types
        type: u1
        repeat: eos
      - id: payload
        size-eos: true