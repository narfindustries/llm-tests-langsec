meta:
  id: sqlite3-db
  file-format: SQLite database file
  endian: le
  encoding: UTF-8

seq:
  - id: magic
    type: str
    size: 16
    enum: [SQLite format 3\0]
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
  - id: pages_in_database
    type: u4
  - id: first freelist page
    type: u4
  - id: freelist page count
    type: u4
  - id: schema_cookie
    type: u4
  - id: schema_format_number
    type: u4
  - id: default page cache size
    type: u4
  - id: largest root btree page
    type: u4
  - id: text encoding
    type: u4
  - id: user_version
    type: u4
  - id: incremental_vacuum_mode
    type: u4
  - id: application_id
    type: u4
  - id: reserved
    type: u4
  - id: version_valid_for
    type: u4
  - id: sqlite_version_number
    type: u4