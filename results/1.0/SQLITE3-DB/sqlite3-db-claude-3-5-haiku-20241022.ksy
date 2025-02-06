meta:
  id: sqlite3_db
  file-extension: db
  endian: le

seq:
  - id: header
    type: sqlite_header
  - id: pages
    type: page_data
    repeat: until
    repeat-until: _io.is_eof()

types:
  sqlite_header:
    seq:
      - id: magic
        contents: [0x53, 0x51, 0x4C, 0x69, 0x74, 0x65, 0x20, 0x66, 0x6F, 0x72, 0x6D, 0x61, 0x74, 0x20, 0x33, 0x00]
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
      - id: default_encoding
        type: u4
        enum: encoding_types
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
      - id: sqlite_version
        type: u4

  page_data:
    seq:
      - id: page_type
        type: u1
        enum: page_types
      - id: first_freeblock
        type: u2
      - id: cell_count
        type: u2
      - id: cell_content_offset
        type: u2
      - id: fragmentated_free_bytes
        type: u1
      - id: cells
        type: cell_data
        repeat: expr
        repeat-expr: cell_count

  cell_data:
    seq:
      - id: payload_size
        type: vlq_base128_le
      - id: row_id
        type: vlq_base128_le
      - id: payload
        size: payload_size.value

  vlq_base128_le:
    seq:
      - id: groups
        type: vlq_group
        repeat: until
        repeat-until: not _.has_next
    instances:
      value:
        value: >-
          _root.calculate_vlq(groups)

  vlq_group:
    seq:
      - id: has_next
        type: b1
      - id: value
        type: b7

enums:
  encoding_types:
    1: utf8
    2: utf16le
    3: utf16be

  page_types:
    0x02: index_interior
    0x05: table_interior
    0x0A: index_leaf
    0x0D: table_leaf

instances:
  calculate_vlq:
    type: u8
    value: |
      (groups) => {
        let result = 0;
        for (let i = 0; i < groups.length; i++) {
          result += groups[i].value << (7 * i);
        }
        return result;
      }