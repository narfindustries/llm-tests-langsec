meta:
  id: sqlite3_db
  file-extension: db
  endian: le
  title: SQLite3 database file format

seq:
  - id: magic
    type: str
    size: 4
    encoding: ascii
  - id: page_size
    type: u2
  - id: write_version
    type: u1
  - id: read_version
    type: u1
  - id: reserved
    type: u1
  - id: max_embedded_payload_fraction
    type: u1
  - id: min_embedded_payload_fraction
    type: u1
  - id: leaf_payload_fraction
    type: u1
  - id: file_format_write_version
    type: u4
  - id: file_format_read_version
    type: u4
  - id: pages
    type: page
    repeat: eos

types:
  page:
    seq:
      - id: page_type
        type: u1
      - id: first_freeblock
        type: u2
      - id: cell_offset
        type: u2
      - id: num_cells
        type: u2
      - id: fragmented_free_bytes
        type: u1
      - id: right_child
        type: u4
        if: page_type == 2 or page_type == 5
      - id: cells
        type: cell
        repeat: expr
        repeat-expr: num_cells

  cell:
    seq:
      - id: payload_length
        type: switch-on
        cases:
          - condition: _parent.page_type == 2
            type: u4
          - condition: _parent.page_type == 5
            type: u4
          - else:
            type: varint
      - id: header_length
        type: varint
      - id: num_columns
        type: varint
      - id: column_data
        type: switch-on
        cases:
          - condition: _parent.page_type == 2
            type: btree_cell
          - condition: _parent.page_type == 5
            type: leaf_cell
          - else:
            type: bytes

  btree_cell:
    seq:
      - id: left_child
        type: u4
      - id: key
        type: bytes
      - id: data
        type: bytes

  leaf_cell:
    seq:
      - id: rowid
        type: varint
      - id: data
        type: bytes

  varint:
    type: switch-on
    cases:
      - condition: _ < 128
        type: u1
      - condition: _ < 16384
        type: u2
      - condition: _ < 1073741824
        type: u3
      - else:
        type: u8

  overflow_page:
    seq:
      - id: next_page
        type: u4
      - id: data
        type: bytes

  free_page:
    seq:
      - id: next_page
        type: u4
      - id: size
        type: u2

  master_journal_header:
    seq:
      - id: magic
        type: str
        size: 4
        encoding: ascii
      - id: version
        type: u4
      - id: page_count
        type: u4
      - id: sector_size
        type: u2
      - id: page_size
        type: u2
      - id: reserved
        type: bytes
        size: 10

  master_journal_page:
    seq:
      - id: page_number
        type: u4
      - id: data
        type: bytes