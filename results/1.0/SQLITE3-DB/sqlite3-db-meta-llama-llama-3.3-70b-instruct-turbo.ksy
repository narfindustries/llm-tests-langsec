meta:
  id: sqlite3_db
  endianness: le
  encoding: ascii
seq:
  - id: magic
    type: str
    size: 4
  - id: file_format_write_version
    type: u1
  - id: file_format_read_version
    type: u1
  - id: reserved_space
    type: u1
  - id: maximum_embedding_size
    type: u1
  - id: file_change_counter
    type: u4le
  - id: pages_in_database
    type: u4le
  - id: first_freelist_page
    type: u4le
  - id: pages
    type: page
    repeat: expr
      value: pages_in_database

types:
  page:
    seq:
      - id: page_type
        type: u1
      - id: first_block_offset
        type: u2le
      - id: page_content_size
        type: u2le
      - id: content
        type:
          switch-on: page_type
          case-0: unused_page
          case-1: btree_page
          case-2: interior_index_page
          case-3: leaf_index_page
          case-4: overflow_page
          case-5: sequential_in_memory_index
          case-6: pointer_map_page

  unused_page:
    seq:
      - id: unused
        type: str
        size: page_content_size

  btree_page:
    seq:
      - id: cell_offset_array
        type: cell_offset
        repeat: expr
          value: page_content_size // 2
      - id: num_cells
        type: u2le
      - id: start_block
        type: u2le
      - id: num_blocks
        type: u2le
      - id: cells
        type: cell
        repeat: expr
          value: num_cells

  cell_offset:
    seq:
      - id: cell_offset
        type: u2le

  cell:
    seq:
      - id: payload_length
        type: u1
      - id: payload
        type: str
        size: payload_length
      - id: rowid
        type: u4le

  interior_index_page:
    seq:
      - id: page_key
        type: str
        size: page_content_size
      - id: child_page_number
        type: u4le

  leaf_index_page:
    seq:
      - id: page_key
        type: str
        size: page_content_size
      - id: leaf_data
        type: str
        size: page_content_size

  overflow_page:
    seq:
      - id: overflow_data
        type: str
        size: page_content_size

  sequential_in_memory_index:
    seq:
      - id: data
        type: str
        size: page_content_size

  pointer_map_page:
    seq:
      - id: page_number
        type: u4le
      - id: ptr_value
        type: u4le