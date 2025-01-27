meta:
  id: sqlite3_db
  file-extension: db
  endian: le
  title: SQLite Database File
  application: SQLite
  license: CC0-1.0
  ks-version: 0.9

doc: |
  SQLite database file format, as used by SQLite 2.x, 3.x, including WAL and 
  rollback journal files.

seq:
  - id: header
    type: file_header

  - id: pages
    type: page
    repeat: eos

types:
  file_header:
    seq:
      - id: signature
        contents: 'SQLite format 3\x00'
      - id: page_size
        type: u2
        doc: The database page size in bytes.
      - id: write_version
        type: u1
        doc: File format write version.
      - id: read_version
        type: u1
        doc: File format read version.
      - id: reserved_space
        type: u1
        doc: Bytes of reserved space at the end of each page.
      - id: max_payload_frac
        type: u1
        doc: Maximum payload fraction per page.
      - id: min_payload_frac
        type: u1
        doc: Minimum payload fraction per page.
      - id: leaf_payload_frac
        type: u1
        doc: Leaf payload fraction per page.
      - id: file_change_counter
        type: u4
        doc: File change counter.
      - id: in_header_db_size
        type: u4
        doc: Size of the database file in pages.
      - id: first_freelist_trunk_page
        type: u4
        doc: Page number of the first freelist trunk page.
      - id: total_freelist_pages
        type: u4
        doc: Total number of freelist pages.
      - id: schema_cookie
        type: u4
        doc: Schema cookie.
      - id: schema_format
        type: u4
        doc: Schema format number.
      - id: default_cache_size
        type: u4
        doc: Default page cache size.
      - id: largest_root_btree_page
        type: u4
        doc: Page number of the largest root b-tree page.
      - id: text_encoding
        type: u4
        enum: encoding
        doc: Text encoding used by this database.
      - id: user_version
        type: u4
        doc: User version.
      - id: increment_vacuum_mode
        type: u4
        doc: Incremental vacuum mode.
      - id: application_id
        type: u4
        doc: Application ID.
      - id: reserved
        size: 20
        doc: Reserved for expansion.
      - id: version_valid_for
        type: u4
        doc: Version valid for number.
      - id: sqlite_version_number
        type: u4
        doc: SQLite version number.

  page:
    seq:
      - id: page_type
        type: u1
        enum: page_type
      - id: first_freeblock
        type: u2
        doc: First freeblock in the page.
      - id: num_cells
        type: u2
        doc: Number of cells in the page.
      - id: cell_content_area
        type: u2
        doc: Offset to cell content area.
      - id: num_fragmented_free_bytes
        type: u1
        doc: Number of fragmented free bytes.
      - id: cells
        type: cell
        repeat: expr
        repeat-expr: num_cells

    types:
      cell:
        seq:
          - id: payload_size
            type: vlq_base128_be
          - id: row_id
            type: vlq_base128_be

enums:
  encoding:
    0x01: utf_8
    0x02: utf_16_le
    0x03: utf_16_be

  page_type:
    0x02: index_leaf
    0x05: table_leaf
    0x0a: index_interior
    0x0d: table_interior

imports:
  - /types/vlq_base128_be