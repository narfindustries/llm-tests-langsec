meta:
  id: sqlite3_db
  endian: le
doc: SQLite3 database file format
seq:
  - id: magic
    size: 16
    type: str
    encoding: ascii
  - id: file_format_version
    size: 2
  - id: size_of_a_database_page
    size: 2
  - id: file_format_write_version
    size: 1
  - id: file_format_read_version
    size: 1
  - id: reserved_space
    size: 20
    type: bytes
  - id: maximum_embedded_payload_fraction
    size: 1
  - id: minimum_embedded_payload_fraction
    size: 1
  - id: leaf_payload_fraction
    size: 1
  - id: file_change_counter
    size: 4
  - id: size_of_the_database_file_in_pages
    size: 4
  - id: first_freelist_trunk_page
    size: 4
  - id: number_of_freelist_pages
    size: 4
  - id: schema_cookie
    size: 4
  - id: schema_format_number
    size: 4
  - id: default_page_cache_size
    size: 4
  - id: largest_root_b_tree_page
    size: 4
  - id: text_encoding
    size: 4
  - id: user_version
    size: 4
  - id: incremental_vacuum_mode
    size: 4
  - id: application_id
    size: 4
  - id: pages
    type: page
    repeat: eos

types:
  page:
    seq:
      - id: page_type
        size: 1
      - id: first_free_block
        size: 2
      - id: number_of_cells
        size: 2
      - id: offset_to_cell_content_area
        size: 2
      - id: number_of_fragmented_free_bytes
        size: 1
      - id: right_child_page
        size: 4
        if: page_type != 2 and page_type != 5
      - id: cells
        type: cell
        repeat:
          repeat-expr: number_of_cells
      - id: payload
        size: eos
        type: payload

  cell:
    seq:
      - id: pointer
        type: varint
      - id: length_of_payload
        type: varint
      - id: varint
        type: varint
      - id: serial_type
        size: 1
      - id: payload
        size: length_of_payload
        type: payload

  payload:
    seq:
      - id: payload_data
        size: eos
        type: bytes

  varint:
    seq:
      - id: value
        type: varint_body

  varint_body:
    seq:
      - id: byte
        size: 1
      - id: more
        size: 1
        if: byte & 0x80 != 0
        type: varint_body

  btree_page:
    seq:
      - id: page
        type: page

  freelist_page:
    seq:
      - id: page
        type: page

  pointer_map_page:
    seq:
      - id: page
        type: page

  wal_frame:
    seq:
      - id: page
        type: page
      - id: wal_frame_header
        type: wal_frame_header

  wal_frame_header:
    seq:
      - id: page_size
        size: 2
      - id: wal_file_header_version
        size: 2
      - id: salt1
        size: 4
      - id: salt2
        size: 4
      - id: checksum1
        size: 4
      - id: checksum2
        size: 4