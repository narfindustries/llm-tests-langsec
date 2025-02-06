meta:
  id: sqlite3
  file-extension: sqlite3
  endian: be
seq:
  - id: header_string
    type: str
    size: 16
    encoding: ASCII
  - id: page_size
    type: u2
  - id: write_version
    type: u1
  - id: read_version
    type: u1
  - id: reserved_space
    type: u1
  - id: max_embedded_payload
    type: u1
  - id: min_embedded_payload
    type: u1
  - id: leaf_payload
    type: u1
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
  - id: default_page_cache
    type: u4
  - id: largest_root_b_tree
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
    type: str
    size: 20
    encoding: ASCII
  - id: version_valid_for
    type: u4
  - id: sqlite_version
    type: u4
types:
  page:
    seq:
      - id: page_type
        type: u1
      - id: data
        size-eos: true
        type: page_data
  page_data:
    seq:
      - id: cells
        type: cell
        repeat: expr
        repeat-expr: _root.page_size / _root.header.page_size
  cell:
    seq:
      - id: payload_size
        type: varint
      - id: payload
        size: payload_size
        type: payload_data
  payload_data:
    seq:
      - id: header_size
        type: varint
      - id: serial_types
        type: serial_type
        repeat: expr
        repeat-expr: header_size
      - id: data
        size-eos: true
        type: data_fields
  serial_type:
    seq:
      - id: type
        type: varint
  data_fields:
    seq:
      - id: fields
        type: data_field
        repeat: expr
        repeat-expr: _parent.header_size
  data_field:
    seq:
      - id: value
        type:
          switch-on: _parent.serial_types[_index].type
          cases:
            0: null_type
            1: s1
            2: s2
            3: s3
            4: s4
            5: s6
            6: s8
            7: f8
            8: u0
            9: u1
            10+: str
        encoding: UTF-8
  varint:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: _.bytes & 0x80 == 0
    instances:
      value:
        value: >
          let shift = 0;
          let result = 0;
          for (let i = 0; i < this.bytes.length; i++) {
            result |= (this.bytes[i] & 0x7F) << shift;
            shift += 7;
          }
          return result;
  null_type:
    seq: []
  u0:
    seq: []
  u1:
    seq: []
  s1:
    seq: []
  s2:
    seq: []
  s3:
    seq: []
  s4:
    seq: []
  s6:
    seq: []
  s8:
    seq: []
  f8:
    seq: []