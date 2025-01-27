meta:
  id: sqlite3_db
  file-extension: db
  endian: le
  title: SQLite Database File
  license: CC0-1.0
  ks-version: 0.9

doc: |
  SQLite is a C library that provides a lightweight disk-based database that doesn’t require
  a separate server process and allows accessing the database using a nonstandard variant
  of the SQL query language. Some applications can use SQLite for internal data storage.
  It’s also possible to prototype an application using SQLite and then port the code to a
  larger database such as PostgreSQL or Oracle.

seq:
  - id: header
    type: file_header

  - id: pages
    type: page
    repeat: expr
    repeat-expr: header.num_pages

types:
  file_header:
    seq:
      - id: magic
        contents: "SQLite format 3\x00"
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
        doc: Bytes of unused "reserved" space at the end of each page.
      - id: max_payload_frac
        type: u1
        doc: Maximum fraction of page used for payload.
      - id: min_payload_frac
        type: u1
        doc: Minimum fraction of page used for payload.
      - id: leaf_payload_frac
        type: u1
        doc: Leaf payload fraction.
      - id: file_change_counter
        type: u4
        doc: File change counter.
      - id: num_pages
        type: u4
        doc: Number of pages in the database.
      - id: first_freelist_trunk_page
        type: u4
        doc: The first freelist trunk page.
      - id: num_freelist_pages
        type: u4
        doc: Number of freelist pages.
      - id: schema_cookie
        type: u4
        doc: The schema cookie.
      - id: schema_format
        type: u4
        doc: Schema format number.
      - id: default_cache_size
        type: u4
        doc: Default page cache size.
      - id: largest_root_btree_page
        type: u4
        doc: Largest root b-tree page.
      - id: text_encoding
        type: u4
        enum: text_encodings
        doc: Text encoding used.
      - id: user_version
        type: u4
        doc: User version.
      - id: incremental_vacuum_mode
        type: u4
        doc: Incremental vacuum mode.
      - id: application_id
        type: u4
        doc: Application ID.
      - id: reserved
        size: 20
        doc: Reserved space for expansion.
      - id: version_valid_for
        type: u4
        doc: Version valid for number.
      - id: sqlite_version_number
        type: u4
        doc: SQLite version number.

  page:
    seq:
      - id: data
        size-eos: true

enums:
  text_encodings:
    1: utf_8
    2: utf_16le
    3: utf_16be