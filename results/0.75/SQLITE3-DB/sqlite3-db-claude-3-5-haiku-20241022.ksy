meta:
  id: sqlite3_db
  file-extension: db
  endian: le

seq:
  - id: header
    type: header
  - id: pages
    type: page
    repeat: eos

types:
  header:
    seq:
      - id: magic_string
        contents: [0x53, 0x51, 0x4C, 0x69, 0x74, 0x65, 0x20, 0x66, 0x6F, 0x72, 0x6D, 0x61, 0x74, 0x20, 0x33, 0x00]
      - id: page_size
        type: u2
      - id: write_version
        type: u1
      - id: read_version
        type: u1
      - id: reserved_space
        size: 20

  page:
    seq:
      - id: page_type
        type: u1
      - id: first_freeblock
        type: u2
      - id: num_cells
        type: u2
      - id: cell_content_start
        type: u2
      - id: fragmented_free_bytes
        type: u1
      - id: cells
        type: cell
        repeat: expr
        repeat-expr: num_cells
    
  cell:
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
    types:
      vlq_group:
        seq:
          - id: has_next
            type: b1
          - id: value
            type: b7
    instances:
      value:
        value: >-
          (groups[0].value & 0b01111111) +
          (groups.size > 1 ? (groups[1].value & 0b01111111) << 7 : 0) +
          (groups.size > 2 ? (groups[2].value & 0b01111111) << 14 : 0) +
          (groups.size > 3 ? (groups[3].value & 0b01111111) << 21 : 0)