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
      - id: max_payload_fraction
        type: u1
      - id: min_payload_fraction
        type: u1
      - id: leaf_payload_fraction
        type: u1
      - id: file_change_counter
        type: u4
      - id: num_pages
        type: u4
      - id: first_freelist_page
        type: u4
      - id: num_free_pages
        type: u4
      - id: schema_cookie
        type: u4
      - id: schema_format
        type: u4
      - id: default_page_cache_size
        type: u4
      - id: largest_mx(MySQL auto-increment) 
        type: u4
      - id: text_encoding
        type: u4
      - id: user_version
        type: u4
      - id: reserved_for_expansion
        size: 20
    instances:
      page_size:
        value: page_size == 1024