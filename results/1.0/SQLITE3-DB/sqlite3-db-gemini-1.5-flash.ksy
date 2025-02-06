types:
  sqlite3_header:
    seq:
      - id: magic_number
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
      - id: text_encoding
        type: u4
      - id: user_version
        type: u4
      - id: application_id
        type: u4
      - id: version_valid_for_all_schema
        type: u1
      - id: file_change_counter
        type: u4
      - id: extra
        type: bytes
        size: 24

  sqlite3_page_header:
    seq:
      - id: page_number
        type: u4
      - id: page_type
        type: u1
      - id: free_block_count
        type: u2
        if: page_type != 0
      - id: unused
        type: u2
        if: page_type != 0
      - id: cells_count
        type: u2
        if: page_type == 5
      - id: first_freeblock_offset
        type: u2
        if: page_type == 5
      - id: cell_content_size
        type: u2
        if: page_type == 5
      - id: rightmost_child_page_number
        type: u4
        if: page_type == 5 and page_type != 1
      - id: checksum
        type: u2
      - id: cell_pointers
        type: array
        type: u2
        size: cells_count
        if: page_type == 5

  sqlite3_cell:
    seq:
      - id: payload_size
        type: u2
      - id: header_size
        type: u2
      - id: payload
        type: bytes
        size: payload_size


  sqlite3_database:
    seq:
      - id: header
        type: sqlite3_header
      - id: pages
        type: array
        type: sqlite3_page
        size: header.max_page_count

  sqlite3_page:
    seq:
      - id: header
        type: sqlite3_page_header
      - id: cells
        type: array
        type: sqlite3_cell
        size: header.cells_count
        if: header.page_type == 5

