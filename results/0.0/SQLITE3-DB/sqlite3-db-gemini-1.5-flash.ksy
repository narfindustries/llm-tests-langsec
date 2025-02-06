types:
  sqlite3_header:
    seq:
      - id: magic
        type: str
        size: 16
      - id: page_size
        type: u2
      - id: write_version
        type: u2
      - id: read_version
        type: u2
      - id: reserved_byte
        type: u1
      - id: max_page_count
        type: u4
      - id: text_encoding
        type: u4
      - id: user_version
        type: u4
      - id: incremental_vacuum_mode
        type: u4
      - id: application_id
        type: u8
      - id: version_valid_for_all_pages
        type: u4
      - id: page_count
        type: u4
      - id: checksum_flag
        type: u4
      - id: free_page_count
        type: u4
      - id: schema_version
        type: u4
      - id: default_page_cache_size
        type: u4
      - id: large_file_support
        type: u4
      - id: empty_page_number
        type: u4
      - id: schema_cookie
        type: u4
      - id: schema_format
        type: u4
      - id: page_size_extension
        type: u4
      - id: reserved_bytes
        type: u4
        size: 5

  sqlite3_page_header:
    seq:
      - id: page_number
        type: u4
      - id: page_type
        type: u1
      - id: free_block_count
        type: u1
      - id: cell_count
        type: u2
      - id: first_free_block
        type: u2
      - id: cell_pointer
        type: u2
      - id: checksum
        type: u4

  sqlite3_cell_header:
    seq:
      - id: header_size
        type: u2
      - id: row_id
        type: u8
      - id: payload_size
        type: u2
      - id: offset_to_payload
        type: u2

  sqlite3_payload:
    seq:
      - id: data
        type: bytes
        size: lambda: self.parent.cell_header.payload_size


  sqlite3_db:
    seq:
      - id: header
        type: sqlite3_header
      - id: pages
        type: sqlite3_page
        repeat: eos

  sqlite3_page:
    seq:
      - id: header
        type: sqlite3_page_header
      - id: cells
        type: sqlite3_cell
        repeat: expr
        expr: self.header.cell_count
      - id: free_blocks
        type: bytes
        repeat: expr
        expr: self.header.free_block_count


  sqlite3_cell:
    seq:
      - id: cell_header
        type: sqlite3_cell_header
      - id: payload
        type: sqlite3_payload

