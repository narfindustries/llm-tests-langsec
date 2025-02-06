meta:
  id: sqlite3_db
  file-extension: db
  endian: le

seq:
  - id: header
    type: file_header
  - id: pages
    type: page
    repeat: eos

types:
  file_header:
    seq:
      - id: magic
        contents: "SQLite format 3\0"
      - id: page_size
        type: u2
      - id: write_version
        type: u1
      - id: read_version
        type: u1
      - id: reserved_bytes
        size: 1
      - id: max_payload_fraction
        type: u1
      - id: min_payload_fraction
        type: u1
      - id: leaf_payload_fraction
        type: u1
      - id: file_change_counter
        type: u4
      - id: database_size_pages
        type: u4
      - id: first_freelist_page
        type: u4
      - id: total_freelist_pages
        type: u4
      - id: schema_cookie
        type: u4
      - id: schema_format
        type: u4
      - id: default_page_cache_size
        type: u4
      - id: largest_root_page
        type: u4
      - id: text_encoding
        type: u4
        enum: text_encoding_enum
      - id: user_version
        type: u4
      - id: incremental_vacuum_mode
        type: u4
      - id: application_id
        type: u4
      - id: reserved_space
        size: 20

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
      - id: payload_length
        type: vlq_base128_le
      - id: row_id
        type: vlq_base128_le
      - id: payload
        size: payload_length
        type: payload

  payload:
    seq:
      - id: header_length
        type: vlq_base128_le
      - id: serial_types
        type: vlq_base128_le
        repeat: until
        repeat-until: _index == header_length.value - 1
      - id: data
        type: data_field(serial_type_index)
        repeat: expr
        repeat-expr: serial_types.size

  data_field:
    params:
      - id: serial_type_index
        type: u4
    seq:
      - id: value
        type:
          switch-on: _parent.serial_types[serial_type_index]
          cases:
            0: null_type
            1: int1_type
            2: int2_type
            3: int3_type
            4: int4_type
            5: int6_type
            6: int8_type
            7: float8_type
            8: zero_type
            9: one_type
            10: internal_type
            11: internal_type
            _: blob_or_text_type

  null_type:
    seq: []
  int1_type:
    seq:
      - id: value
        type: s1
  int2_type:
    seq:
      - id: value
        type: s2
  int3_type:
    seq:
      - id: value
        type: s3
  int4_type:
    seq:
      - id: value
        type: s4
  int6_type:
    seq:
      - id: value
        type: s6
  int8_type:
    seq:
      - id: value
        type: s8
  float8_type:
    seq:
      - id: value
        type: f8
  zero_type:
    seq: []
  one_type:
    seq: []
  internal_type:
    seq: []
  blob_or_text_type:
    params:
      - id: serial_type
        type: u1
    seq:
      - id: length
        type: u1
      - id: value
        type: str
        size: (serial_type - 12) / 2
        encoding: utf-8

enums:
  text_encoding_enum:
    1: utf8
    2: utf16le
    3: utf16be