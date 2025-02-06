meta:
  id: sqlite3
  file-extension: db
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
        contents: [0x53, 0x51, 0x4C, 0x69, 0x74, 0x65, 0x20, 0x66, 0x6F, 0x72, 0x6D, 0x61, 0x74, 0x20, 0x33, 0x00]
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

  btree_page_header:
    seq:
      - id: page_type
        type: u1
        enum: page_type
      - id: first_freeblock
        type: u2
      - id: number_of_cells
        type: u2
      - id: cell_content_area
        type: u2
      - id: fragmented_free_bytes
        type: u1
      - id: right_child_pointer
        type: u4
        if: "page_type == page_type::interior_index or page_type == page_type::interior_table"

  cell:
    seq:
      - id: payload_size
        type: varint
      - id: row_id
        type: varint
        if: "_parent.page_header.page_type == page_type::leaf_table"
      - id: left_child_pointer
        type: u4
        if: "_parent.page_header.page_type == page_type::interior_index or _parent.page_header.page_type == page_type::interior_table"
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
        value: >
          bytes.size == 1 ? bytes[0] & 0x7F :
          bytes.size == 2 ? ((bytes[0] & 0x7F) << 7) | (bytes[1] & 0x7F) :
          bytes.size == 3 ? ((bytes[0] & 0x7F) << 14) | ((bytes[1] & 0x7F) << 7) | (bytes[2] & 0x7F) :
          bytes.size == 4 ? ((bytes[0] & 0x7F) << 21) | ((bytes[1] & 0x7F) << 14) | ((bytes[2] & 0x7F) << 7) | (bytes[3] & 0x7F) :
          bytes.size == 5 ? ((bytes[0] & 0x7F) << 28) | ((bytes[1] & 0x7F) << 21) | ((bytes[2] & 0x7F) << 14) | ((bytes[3] & 0x7F) << 7) | (bytes[4] & 0x7F) :
          bytes.size == 6 ? ((bytes[0] & 0x7F) << 35) | ((bytes[1] & 0x7F) << 28) | ((bytes[2] & 0x7F) << 21) | ((bytes[3] & 0x7F) << 14) | ((bytes[4] & 0x7F) << 7) | (bytes[5] & 0x7F) :
          bytes.size == 7 ? ((bytes[0] & 0x7F) << 42) | ((bytes[1] & 0x7F) << 35) | ((bytes[2] & 0x7F) << 28) | ((bytes[3] & 0x7F) << 21) | ((bytes[4] & 0x7F) << 14) | ((bytes[5] & 0x7F) << 7) | (bytes[6] & 0x7F) :
          bytes.size == 8 ? ((bytes[0] & 0x7F) << 49) | ((bytes[1] & 0x7F) << 42) | ((bytes[2] & 0x7F) << 35) | ((bytes[3] & 0x7F) << 28) | ((bytes[4] & 0x7F) << 21) | ((bytes[5] & 0x7F) << 14) | ((bytes[6] & 0x7F) << 7) | (bytes[7] & 0x7F) :
          ((bytes[0] & 0x7F) << 56) | ((bytes[1] & 0x7F) << 49) | ((bytes[2] & 0x7F) << 42) | ((bytes[3] & 0x7F) << 35) | ((bytes[4] & 0x7F) << 28) | ((bytes[5] & 0x7F) << 21) | ((bytes[6] & 0x7F) << 14) | ((bytes[7] & 0x7F) << 7) | (bytes[8] & 0x7F)

enums:
  page_type:
    0x02: interior_index
    0x05: interior_table
    0x0a: leaf_index
    0x0d: leaf_table
    0x00: overflow

  text_encoding:
    1: utf8
    2: utf16le
    3: utf16be