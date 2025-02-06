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
        contents: "SQLite format 3\0"
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
      - id: db_size_in_pages
        type: u4
      - id: first_freelist_page
        type: u4
      - id: freelist_pages_count
        type: u4
      - id: schema_cookie
        type: u4
      - id: schema_format_number
        type: u4
      - id: default_page_cache_size
        type: u4
      - id: largest_root_btree_page
        type: u4
      - id: text_encoding
        type: u4
      - id: user_version
        type: s4
      - id: incremental_vacuum_mode
        type: u4
      - id: application_id
        type: u4
      - id: reserved_expansion
        size: 20
      - id: version_valid_for
        type: u4
      - id: sqlite_version_number
        type: u4
  page:
    seq:
      - id: page_type
        type: u1
      - id: content
        size: _root.header.page_size - 1
  cell:
    seq:
      - id: cell_type
        type: u1
      - id: payload
        size-eos: true
  record:
    seq:
      - id: serial_type
        type: vlq_base128_le
      - id: value
        size: _root.header.page_size
  vlq_base128_le:
    seq:
      - id: byte
        type: u1
        repeat: until
        repeat-until: _ == 0x80