meta:
  id: sqlite3_db
  title: SQLite3 Database File Format
  application:
    - SQLite
  xref:
    sqlite: "https://www.sqlite.org/fileformat2.html"
  endian: be

seq:
  - id: header
    type: header

types:
  header:
    seq:
      - id: magic_header_string
        contents: "SQLite format 3\0"
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
      - id: database_size_pages
        type: u4
      - id: first_freelist_trunk_page
        type: u4
      - id: total_freelist_pages
        type: u4
      - id: schema_cookie
        type: u4
      - id: schema_format_number
        type: u4
      - id: default_page_cache_size
        type: u4
      - id: largest_root_btree_page
        type: u4
      - id: text_encoding
        type: u4
        enum: text_encoding
      - id: user_version
        type: u4
      - id: incremental_vacuum_mode
        type: u4
      - id: application_id
        type: u4
      - id: reserved
        size: 20
      - id: version_valid_number
        type: u4
      - id: sqlite_version_number
        type: u4

  page:
    seq:
      - id: page_type
        type: u1
        enum: page_type
      - id: first_freeblock_offset
        type: u2
      - id: cells_count
        type: u2
      - id: cell_content_area_start
        type: u2
      - id: fragmented_free_bytes
        type: u1
      - id: rightmost_pointer
        type: u4
        if: page_type == page_type::interior_index or page_type == page_type::interior_table

enums:
  page_type:
    0x02: interior_index
    0x05: interior_table
    0x0A: leaf_index
    0x0D: leaf_table

  text_encoding:
    1: utf8
    2: utf16le
    3: utf16be

instances:
  first_page:
    pos: 100
    type: page