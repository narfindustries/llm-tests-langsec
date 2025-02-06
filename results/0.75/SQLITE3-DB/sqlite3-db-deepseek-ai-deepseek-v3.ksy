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
      - id: page_size
        type: u2
      - id: write_version
        type: u1
      - id: read_version
        type: u1
      - id: reserved_bytes
        type: u1
      - id: max_embedded_payload_frac
        type: u1
      - id: min_embedded_payload_frac
        type: u1
      - id: leaf_payload_frac
        type: u1
      - id: file_change_counter
        type: u4
      - id: db_size_in_pages
        type: u4
      - id: first_free_page
        type: u4
      - id: num_free_pages
        type: u4
      - id: schema_cookie
        type: u4
      - id: schema_format_number
        type: u4
      - id: default_page_cache_size
        type: u4
      - id: largest_btree_page
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
      - id: free_block_offset
        type: u2
      - id: num_cells
        type: u2
      - id: first_cell_offset
        type: u2
      - id: fragmented_free_bytes
        type: u1
      - id: right_child_pointer
        type: u4
        if: page_type == 0x02 or page_type == 0x05
      - id: cells
        type: cell
        repeat: expr
        repeat-expr: num_cells
  cell:
    seq:
      - id: len_payload
        type: u4
      - id: rowid
        type: u8
      - id: payload
        size: len_payload
  vlq_base128_le:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: _ & 0x80 == 0