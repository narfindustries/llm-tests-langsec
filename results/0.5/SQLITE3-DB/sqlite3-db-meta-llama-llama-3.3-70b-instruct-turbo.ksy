meta:
  id: sqlite3-db
  file-extension: db
  endian: le

seq:
  - id: header
    type: header

types:
  header:
    seq:
      - id: magic
        size: 16
      - id: page_size
        type: u2
      - id: write_version
        type: u1
      - id: read_version
        type: u1
      - id: reserved_space
        type: u1
      - id: maxembedding
        type: u1
      - id: min_pages_per_map
        type: u1
      - id: leaf_payload_fraction
        type: u1
      - id: file_format_version
        type: u1