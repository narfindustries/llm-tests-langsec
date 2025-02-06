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
        valid:
          any-of: [512, 1024, 2048, 4096, 8192, 16384, 32768, 65536]
      - id: write_version
        type: u1
        valid:
          any-of: [1, 2]
      - id: read_version
        type: u1
        valid:
          any-of: [1, 2]
      - id: reserved_space
        type: u1
      - id: max_payload_fraction
        type: u1
        default: 64
      - id: min_payload_fraction
        type: u1
        default: 32
      - id: leaf_payload_fraction
        type: u1
        default: 32
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
        valid:
          any-of: [1, 2, 3, 4]
      - id: default_encoding
        type: u4
        enum: encoding_type
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
        enum: page_type
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
        type: payload
        size: payload_length.value

  payload:
    seq:
      - id: column_count
        type: vlq_base128_le
      - id: column_types
        type: vlq_base128_le
        repeat: expr
        repeat-expr: column_count.value
      - id: column_data
        type: column
        repeat: expr
        repeat-expr: column_count.value

  column:
    seq:
      - id: data
        type:
          switch-on: _parent.column_types[_index]
          cases:
            0: empty
            1: s1
            2: s2
            3: s3
            4: s4
            5: s6
            6: s8
            7: f8
            8: string
            9: blob

  empty:
    seq: []
  s1:
    seq:
      - id: value
        type: s1
  s2:
    seq:
      - id: value
        type: s2
  s3:
    seq:
      - id: value
        type: s3
  s4:
    seq:
      - id: value
        type: s4
  s6:
    seq:
      - id: value
        type: s6
  s8:
    seq:
      - id: value
        type: s8
  f8:
    seq:
      - id: value
        type: f8
  string:
    seq:
      - id: length
        type: vlq_base128_le
      - id: value
        type: str
        size: length.value
        encoding: UTF-8
  blob:
    seq:
      - id: length
        type: vlq_base128_le
      - id: value
        type: bytes
        size: length.value

enums:
  page_type:
    0x02: interior_index
    0x05: interior_table
    0x0A: leaf_index
    0x0D: leaf_table

  encoding_type:
    1: utf8
    2: utf16le
    3: utf16be