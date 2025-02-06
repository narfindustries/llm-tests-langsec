meta:
  id: sqlite3
  title: SQLite3 database file
  file-extension: 
    - db
    - sqlite
    - sqlite3
  endian: be

seq:
  - id: header
    type: database_header
  - id: pages
    type: page
    repeat: eos

types:
  database_header:
    seq:
      - id: magic
        contents: ['SQLite format 3', 0]
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
      - id: largest_root_btree
        type: u4
      - id: text_encoding
        type: u4
        enum: encoding
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
      - id: page_header
        type: btree_page_header
      - id: cells
        type: cell
        repeat: expr
        repeat-expr: page_header.number_of_cells
      - id: cell_pointers
        type: u2
        repeat: expr
        repeat-expr: page_header.number_of_cells

  btree_page_header:
    seq:
      - id: page_type
        type: u1
      - id: first_freeblock
        type: u2
      - id: number_of_cells
        type: u2
      - id: cell_content_area
        type: u2
      - id: fragmented_free_bytes
        type: u1
      - id: rightmost_pointer
        type: u4
        if: "page_type == 2 or page_type == 5"

  cell:
    seq:
      - id: payload_size
        type: varint
      - id: row_id
        type: varint
        if: "_parent.page_header.page_type == 0x0d"
      - id: payload
        size: payload_size.value

  varint:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: '(_ & 0x80) == 0'
    instances:
      value:
        value: >-
          bytes.size == 1 ? bytes[0] & 0x7f :
          bytes.size == 2 ? ((bytes[0] & 0x7f) << 7) | (bytes[1] & 0x7f) :
          bytes.size == 3 ? ((bytes[0] & 0x7f) << 14) | ((bytes[1] & 0x7f) << 7) | (bytes[2] & 0x7f) :
          bytes.size == 4 ? ((bytes[0] & 0x7f) << 21) | ((bytes[1] & 0x7f) << 14) | ((bytes[2] & 0x7f) << 7) | (bytes[3] & 0x7f) :
          bytes.size == 5 ? ((bytes[0] & 0x7f) << 28) | ((bytes[1] & 0x7f) << 21) | ((bytes[2] & 0x7f) << 14) | ((bytes[3] & 0x7f) << 7) | (bytes[4] & 0x7f) :
          bytes.size == 6 ? ((bytes[0] & 0x7f) << 35) | ((bytes[1] & 0x7f) << 28) | ((bytes[2] & 0x7f) << 21) | ((bytes[3] & 0x7f) << 14) | ((bytes[4] & 0x7f) << 7) | (bytes[5] & 0x7f) :
          bytes.size == 7 ? ((bytes[0] & 0x7f) << 42) | ((bytes[1] & 0x7f) << 35) | ((bytes[2] & 0x7f) << 28) | ((bytes[3] & 0x7f) << 21) | ((bytes[4] & 0x7f) << 14) | ((bytes[5] & 0x7f) << 7) | (bytes[6] & 0x7f) :
          bytes.size == 8 ? ((bytes[0] & 0x7f) << 49) | ((bytes[1] & 0x7f) << 42) | ((bytes[2] & 0x7f) << 35) | ((bytes[3] & 0x7f) << 28) | ((bytes[4] & 0x7f) << 21) | ((bytes[5] & 0x7f) << 14) | ((bytes[6] & 0x7f) << 7) | (bytes[7] & 0x7f) :
          ((bytes[0] & 0x7f) << 56) | ((bytes[1] & 0x7f) << 49) | ((bytes[2] & 0x7f) << 42) | ((bytes[3] & 0x7f) << 35) | ((bytes[4] & 0x7f) << 28) | ((bytes[5] & 0x7f) << 21) | ((bytes[6] & 0x7f) << 14) | ((bytes[7] & 0x7f) << 7) | (bytes[8] & 0x7f)

enums:
  encoding:
    1: utf8
    2: utf16le
    3: utf16be

  page_type:
    2: interior_index
    5: interior_table
    10: leaf_index
    13: leaf_table