meta:
  id: sqlite3_db
  file-extension: db
  endian: le

seq:
  - id: header
    type: file_header
  - id: pages
    type: page
    repeat: expr
    repeat-expr: header.database_size

types:
  file_header:
    seq:
      - id: magic
        contents: "SQLite format 3\x00"
      - id: page_size
        type: u2
        valid:
          min: 512
          max: 65536
      - id: file_change_counter
        type: u4
      - id: database_size
        type: u4
      - id: first_freelist_page
        type: u4
      - id: freelist_page_count
        type: u4
      - id: schema_cookie
        type: u4
      - id: schema_format
        type: u4
        enum: schema_format_enum
      - id: default_page_cache_size
        type: u4
      - id: largest_root_page
        type: u4
      - id: text_encoding
        type: u4
        enum: text_encoding_enum
      - id: user_version
        type: u2
      - id: incremental_vacuum_mode
        type: u1
        enum: vacuum_mode_enum

  page:
    seq:
      - id: page_type
        type: u1
        enum: page_type_enum
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
        type: vlq_base128_le
      - id: row_id
        type: vlq_base128_le
      - id: payload
        size: payload_size.value
        type: payload

  payload:
    seq:
      - id: serial_types
        type: u1
        repeat: until
        repeat-until: _ == 0
      - id: data
        type: data_value
        repeat: expr
        repeat-expr: serial_types.size

  data_value:
    params:
      - id: serial_type
        type: u1
    seq:
      - id: value
        type:
          switch-on: serial_type
          cases:
            0: null_value
            1: s_int1
            2: s_int2
            3: s_int3
            4: s_int4
            5: s_int6
            6: s_int8
            7: float_value
            8: zero_value
            9: one_value
            10: u_int1
            11: u_int2

  null_value:
    seq: []

  s_int1:
    seq:
      - id: value
        type: s1

  s_int2:
    seq:
      - id: value
        type: s2

  s_int3:
    seq:
      - id: value
        type: s3

  s_int4:
    seq:
      - id: value
        type: s4

  s_int6:
    seq:
      - id: value
        type: s6

  s_int8:
    seq:
      - id: value
        type: s8

  float_value:
    seq:
      - id: value
        type: f8

  zero_value:
    seq: []

  one_value:
    seq: []

  u_int1:
    seq:
      - id: value
        type: u1

  u_int2:
    seq:
      - id: value
        type: u2

enums:
  schema_format_enum:
    1: legacy
    2: wal
    3: transactional
    4: modern

  text_encoding_enum:
    1: utf8
    2: utf16le
    3: utf16be

  vacuum_mode_enum:
    0: disabled
    1: enabled

  page_type_enum:
    2: index_interior
    5: table_interior
    10: index_leaf
    13: table_leaf