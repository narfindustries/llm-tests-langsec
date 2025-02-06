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
        type: u1
      - id: max_payload_fraction
        type: u1
      - id: min_payload_fraction
        type: u1
      - id: leaf_payload_fraction
        type: u1
      - id: file_change_counter
        type: u4
      - id: database_page_count
        type: u4
      - id: first_freelist_page
        type: u4
      - id: total_freelist_pages
        type: u4
      - id: schema_cookie
        type: u4
      - id: schema_format
        type: u4
      - id: default_cache_size
        type: u4
      - id: largest_root_page
        type: u4
      - id: text_encoding
        type: u4
        enum: text_encodings
      - id: user_version
        type: u4
      - id: incremental_vacuum_mode
        type: u4
      - id: application_id
        type: u4
      - id: reserved
        size: 20

  page:
    seq:
      - id: page_type
        type: u1
        enum: page_types
      - id: first_free_block
        type: u2
      - id: cell_count
        type: u2
      - id: cell_pointer_array_start
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
        size: payload_length.value
        type: record

  record:
    seq:
      - id: header_length
        type: vlq_base128_le
      - id: serial_types
        type: vlq_base128_le
        repeat: until
        repeat-until: _index * 8 >= header_length.value * 8
      - id: data_fields
        type: data_field(serial_type_index)
        repeat: expr
        repeat-expr: serial_types.size
    instances:
      serial_type_index:
        value: '_root.serial_type_index'

  data_field:
    params:
      - id: serial_type_index
        type: u4
    seq:
      - id: value
        type:
          switch-on: _parent.serial_types[serial_type_index].value
          cases:
            0: sqlite_null
            1: sqlite_int1
            2: sqlite_int2
            3: sqlite_int3
            4: sqlite_int4
            5: sqlite_int6
            6: sqlite_int8
            7: sqlite_float
            8: sqlite_zero
            9: sqlite_one
            10: sqlite_reserved1
            11: sqlite_reserved2
            12: sqlite_blob_0
            13: sqlite_text_0

  vlq_base128_le:
    seq:
      - id: groups
        type: group
        repeat: until
        repeat-until: not _.has_next

  group:
    seq:
      - id: has_next
        type: b1
      - id: value
        type: b7

  sqlite_null:
    seq: []

  sqlite_int1:
    seq:
      - id: value
        type: s1

  sqlite_int2:
    seq:
      - id: value
        type: s2

  sqlite_int3:
    seq:
      - id: value
        type: s3

  sqlite_int4:
    seq:
      - id: value
        type: s4

  sqlite_int6:
    seq:
      - id: value
        type: s6

  sqlite_int8:
    seq:
      - id: value
        type: s8

  sqlite_float:
    seq:
      - id: value
        type: f8

  sqlite_zero:
    seq:
      - id: value
        contents: [0]

  sqlite_one:
    seq:
      - id: value
        contents: [1]

  sqlite_reserved1:
    seq: []

  sqlite_reserved2:
    seq: []

  sqlite_blob_0:
    seq:
      - id: length
        type: vlq_base128_le
      - id: data
        size: length.value

  sqlite_text_0:
    seq:
      - id: length
        type: vlq_base128_le
      - id: data
        type: str
        size: length.value
        encoding: utf-8

enums:
  text_encodings:
    1: utf8
    2: utf16le
    3: utf16be

  page_types:
    2: interior_index
    5: interior_table
    10: leaf_index
    13: leaf_table

instances:
  serial_type_index:
    value: 0