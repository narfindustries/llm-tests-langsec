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
      - id: header_string
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
      - id: freelist_pages
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
      - id: sqlite_version
        type: u4

  page:
    seq:
      - id: page_header
        type: page_header
      - id: cell_pointers
        type: u2
        repeat: expr
        repeat-expr: page_header.number_of_cells
      - id: cells
        type: cell(_index)
        repeat: expr
        repeat-expr: page_header.number_of_cells

  page_header:
    seq:
      - id: page_type
        type: u1
      - id: first_freeblock
        type: u2
      - id: number_of_cells
        type: u2
      - id: cell_content_offset
        type: u2
      - id: fragmented_free_bytes
        type: u1
      - id: right_child_pointer
        type: u4
        if: "page_type == 0x02 or page_type == 0x05"

  cell:
    params:
      - id: cell_index
        type: s4
    seq:
      - id: payload_length
        type: varint
      - id: row_id
        type: varint
        if: "_parent.page_header.page_type == 0x0D"
      - id: payload
        size: payload_length.value
      - id: overflow_page
        type: u4
        if: "payload_length.value > (_parent.page_header.page_type == 0x0D ? 0 : payload_length.value)"

  varint:
    seq:
      - id: byte1
        type: u1
      - id: byte2
        type: u1
        if: "byte1 & 0x80 != 0"
      - id: byte3
        type: u1
        if: "byte2 & 0x80 != 0"
      - id: byte4
        type: u1
        if: "byte3 & 0x80 != 0"
      - id: byte5
        type: u1
        if: "byte4 & 0x80 != 0"
      - id: byte6
        type: u1
        if: "byte5 & 0x80 != 0"
      - id: byte7
        type: u1
        if: "byte6 & 0x80 != 0"
      - id: byte8
        type: u1
        if: "byte7 & 0x80 != 0"
      - id: byte9
        type: u1
        if: "byte8 & 0x80 != 0"
    instances:
      value:
        value: |
          ((byte1 & 0x7f)) |
          ((byte2 & 0x7f) << 7) |
          ((byte3 & 0x7f) << 14) |
          ((byte4 & 0x7f) << 21) |
          ((byte5 & 0x7f) << 28) |
          ((byte6 & 0x7f) << 35) |
          ((byte7 & 0x7f) << 42) |
          ((byte8 & 0x7f) << 49) |
          ((byte9 & 0x7f) << 56)

enums:
  page_types:
    0x02: btree_interior_index
    0x05: btree_interior_table
    0x0a: btree_leaf_index
    0x0d: btree_leaf_table
    0x00: overflow
    0x01: freelist
    0x07: pointer_map

  encoding:
    1: utf8
    2: utf16le
    3: utf16be