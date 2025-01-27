meta:
  id: sqlite3_db
  file-extension: db
  endian: le
  title: SQLite Database File
  license: CC0-1.0
doc: |
  SQLite is a C library that provides a lightweight disk-based database that doesn’t require a separate server process and allows accessing the database using a nonstandard variant of the SQL query language. Some applications can use SQLite for internal data storage. It’s also possible often to find an SQLite database as part of the application resources. For example, in iOS applications, SQLite is used to store the application data in an efficient way. SQLite databases typically use the `.db` file extension and are also known by other extensions like `.sqlite`, `.db3`, and `.sqlite3`.
doc-ref: https://www.sqlite.org/fileformat.html
seq:
  - id: header
    type: file_header
  - id: pages
    type: page
    repeat: eos
types:
  file_header:
    seq:
      - id: magic
        contents: "SQLite format 3\x00"
      - id: page_size
        type: u2
        doc: The database page size in bytes. Must be a power of two between 512 and 32768 inclusive, or the value 1 representing a page size of 65536.
      - id: write_version
        type: u1
        enum: write_version
        doc: File format write version. 1 for legacy; 2 for WAL.
      - id: read_version
        type: u1
        enum: write_version
        doc: File format read version. 1 for legacy; 2 for WAL.
      - id: reserved_space
        type: u1
        doc: Bytes of unused "reserved" space at the end of each page. Usually 0.
      - id: max_payload_frac
        type: u1
        doc: Maximum fraction of page space that can be used to store a BLOB or string. Must be at least 32.
      - id: min_payload_frac
        type: u1
        doc: Minimum fraction of page space that can be used to store a BLOB or string. Must be no more than 255.
      - id: leaf_payload_frac
        type: u1
        doc: Fraction of page space to reserve for the largest BLOB or string. Must be no more than 255.
      - id: file_change_counter
        type: u4
        doc: This value is incremented when the database file is changed.
      - id: num_pages
        type: u4
        doc: The total number of pages in the database file.
      - id: first_freelist_trunk_page
        type: u4
        doc: The page number of the first freelist trunk page.
      - id: num_freelist_pages
        type: u4
        doc: The total number of freelist pages.
      - id: schema_cookie
        type: u4
        doc: This value is incremented when the database schema is changed.
      - id: schema_format
        type: u4
        doc: Indicates the schema format number. 1, 2, 3 or 4.
      - id: default_cache_size
        type: u4
        doc: Suggested number of database cache pages to use.
      - id: largest_root_page
        type: u4
        doc: The page number of the largest root page.
      - id: text_encoding
        type: u4
        enum: text_encoding
        doc: The database text encoding.
      - id: user_version
        type: u4
        doc: User version number, can be used by the application.
      - id: increment_version
        type: u4
        doc: Incremental vacuum mode.
      - id: application_id
        type: u4
        doc: The "Application ID" set by PRAGMA.
      - id: reserved
        size: 20
        doc: Reserved space for expansion.
      - id: version_valid_for
        type: u4
        doc: Version of the database, set by PRAGMA.
      - id: sqlite_version
        type: u4
        doc: The SQLite version number.
  page:
    seq:
      - id: page_type
        type: u1
        enum: page_type
      - id: first_freeblock
        type: u2
        doc: Offset to first freeblock
      - id: num_cells
        type: u2
        doc: Number of cells on the page
      - id: cell_content_offset
        type: u2
        doc: Offset to start of cell content area
      - id: num_frag_free_bytes
        type: u1
        doc: Number of fragmented free bytes within the cell content area
      - id: right_most_pointer
        type: u4
        doc: Right most pointer if interior page
      - id: cells
        type: cell
        repeat: expr
        repeat-expr: num_cells
  cell:
    seq:
      - id: left_child_page
        type: u4
        doc: Left child page number
      - id: row_id
        type: u8
        doc: Row ID
enums:
  write_version:
    1: legacy
    2: wal
  text_encoding:
    1: utf_8
    2: utf_16le
    3: utf_16be
  page_type:
    2: index_interior
    5: table_interior
    10: index_leaf
    13: table_leaf