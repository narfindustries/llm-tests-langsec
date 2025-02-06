type: struct
seq:
  - id: file_format
    type: str
    size: 16
  - id: page_size
    type: u2
  - id: write_version
    type: u2
  - id: read_version
    type: u2
  - id: reserved_space
    type: u4
  - id: max_page_count
    type: u4
  - id: change_count
    type: u4
  - id: sector_size
    type: u4
  - id: file_format_unused
    type: u1
    size: 20
  - id: schema_cookie
    type: u4
  - id: schema_format
    type: u4
  - id: default_page_cache_size
    type: u4
  - id: largest_btree_page
    type: u4
  - id: unused_1
    type: u4
  - id: text_encoding
    type: u4
  - id: user_version
    type: u4
  - id: incremental_vacuum_mode
    type: u4
  - id: application_id
    type: u8
  - id: version_valid_for_all_schema_changes
    type: u1
  - id: unused_2
    type: u1
    size: 3
  - id: pages
    type: seq
    size: max_page_count
    elemType: page

types:
  page:
    seq:
      - id: page_header
        type: page_header
      - id: page_data
        type: bytes
        size: (page_size - page_header_size)

  page_header:
    seq:
      - id: page_type
        type: u1
      - id: page_number
        type: u4
      - id: flags
        type: u1
      - id: free_bytes
        type: u2
      - id: cell_pointer_count
        type: u2
      - id: cell_pointer_offset
        type: u2
      - id: unused_3
        type: u2
      - id: right_most_child_page_number
        type: u4
      - id: unused_4
        type: u4

