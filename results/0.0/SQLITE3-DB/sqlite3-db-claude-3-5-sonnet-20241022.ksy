meta:
  id: sqlite3
  file-extension: db
  endian: be

seq:
  - id: header
    type: database_header
  - id: pages
    type: page
    repeat: eos

types:
  database_header:
    seq:
      - id: magic
        contents: ['SQLite format 3', 0]
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
      - id: database_size
        type: u4
      - id: first_freelist_trunk_page
        type: u4
      - id: num_freelist_pages
        type: u4
      - id: schema_cookie
        type: u4
      - id: schema_format
        type: u4
      - id: default_page_cache_size
        type: u4
      - id: largest_root_btree_page
        type: u4
      - id: text_encoding
        type: u4
        enum: encoding
      - id: user_version
        type: u4
      - id: vacuum_mode
        type: u4
      - id: application_id
        type: u4
      - id: reserved
        size: 20
      - id: version_valid_for
        type: u4
      - id: sqlite_version
        type: u4

  page:
    seq:
      - id: page_type
        type: u1
        enum: page_type
      - id: first_freeblock
        type: u2
      - id: num_cells
        type: u2
      - id: cell_content_area
        type: u2
      - id: fragmented_free_bytes
        type: u1
      - id: right_child_pointer
        type: u4
        if: "page_type == page_type::interior_index or page_type == page_type::interior_table"
      - id: cells
        type: cell
        repeat: expr
        repeat-expr: num_cells

  cell:
    seq:
      - id: cell_type
        type: u1
      - id: len_payload
        type: u4
      - id: row_id
        type: u4
        if: "_parent.page_type == page_type::leaf_table"
      - id: payload
        size: len_payload
        type: record
        if: "_parent.page_type == page_type::leaf_table or _parent.page_type == page_type::leaf_index"
      - id: page_number
        type: u4
        if: "_parent.page_type == page_type::interior_table or _parent.page_type == page_type::interior_index"
      - id: key
        type: u4
        if: "_parent.page_type == page_type::interior_table"

  record:
    seq:
      - id: header_size
        type: u4
      - id: column_types
        type: u1
        repeat: expr
        repeat-expr: header_size
      - id: values
        type: column_value
        repeat: expr
        repeat-expr: header_size

  column_value:
    seq:
      - id: value
        type:
          switch-on: _parent.column_types[_index]
          cases:
            0: null_type
            1: u1
            2: u2
            3: bytes_3
            4: u4
            5: bytes_6
            6: u8
            7: f8
            8: const_zero
            9: const_one
            _: blob_or_text(_parent.column_types[_index])

  null_type: {}
  const_zero: {}
  const_one: {}
  
  bytes_3:
    seq:
      - id: value
        size: 3
  
  bytes_6:
    seq:
      - id: value
        size: 6

  blob_or_text:
    params:
      - id: type_code
        type: u1
    seq:
      - id: value
        size: (type_code - 12) / 2

enums:
  encoding:
    1: utf8
    2: utf16le
    3: utf16be

  page_type:
    0x02: interior_index
    0x05: interior_table
    0x0a: leaf_index
    0x0d: leaf_table
    0x00: overflow_or_freelist