meta:
  id: sqlite3-db
  file-extension: db
  endian: le
seq:
  - id: magic
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
  - id: max_embedded_payload_fraction
    type: u1
  - id: min_embedded_payload_fraction
    type: u1
  - id: leaf_payload_fraction
    type: u1
  - id: file_change_counter
    type: u4
  - id: pages_in_file
    type: u4
  - id: first_freelist_page
    type: u4
  - id: freelist_page_count
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
  - id: incremental_vacuum_mode
    type: u4
  - id: application_id
    type: u4
  - id: reserved_for_expansion
    type: u4
    repeat: 20
  - id: version_valid_for
    type: u4
  - id: sqlite_version_number
    type: u4
types:
  btree_page: &btree_page
    seq:
      - id: page_type
        type: u1
      - id: first_freeblock
        type: u2
      - id: num_cells
        type: u2
      - id: cells
        type: cell
        repeat: num_cells
      - id: freeblocks
        type: freeblock
        repeat: expr
  cell: &cell
    seq:
      - id: length
        type: u2
      - id: payload
        type: str
        size: length
        encoding: UTF-8
  freeblock: &freeblock
    seq:
      - id: offset
        type: u2
      - id: size
        type: u2