seq:
  - id: magic
    size: 16
    type: str
    encoding: ascii
  - id: page_size
    size: 2
    type: int
  - id: write_version
    size: 1
    type: int
  - id: read_version
    size: 1
    type: int
  - id: reserved_space
    size: 1
    type: int
  - id: max_embedded_payload_fraction
    size: 1
    type: int
  - id: min_embedded_payload_fraction
    size: 1
    type: int
  - id: leaf_payload_fraction
    size: 1
    type: int
  - id: file_change_counter
    size: 4
    type: int
  - id: size_in_pages
    size: 4
    type: int
  - id: first_freelist_trunk_page
    size: 4
    type: int
  - id: number_of_freelist_pages
    size: 4
    type: int
  - id: schema_cookie
    size: 4
    type: int
  - id: schema_format_number
    size: 4
    type: int
  - id: default_page_cache_size
    size: 4
    type: int
  - id: largest_root_btree_page_size
    size: 4
    type: int
  - id: text_encoding
    size: 4
    type: int
  - id: user_version
    size: 4
    type: int
  - id: incremental_vacuum_mode
    size: 4
    type: int
  - id: application_id
    size: 4
    type: int
  - id: reserved_for_expansion
    size: 20
    type: bytes
  - id: pages
    type: page
    repeat: expr
    repeat-expr: this.size_in_pages

types:
  page:
    seq:
      - id: page_type
        size: 1
        type: int
      - id: first_freeblock_offset
        size: 2
        type: int
      - id: number_of_cells
        size: 2
        type: int
      - id: start_of_content_offset
        size: 2
        type: int
      - id: fragmented_free_bytes
        size: 1
        type: int
      - id: cells
        type: cell
        repeat: expr
        repeat-expr: this.number_of_cells
      - id: freeblocks
        type: freeblock
        repeat: until
        repeat-until: _io.pos >= _io.size - 1

    instances:
      freeblock:
        seq:
          - id: offset
            size: 2
            type: int
          - id: size
            size: 2
            type: int
        size: expr
        size-expr: this.size

      cell:
        seq:
          - id: payload_length
            size: varint
            type: int
          - id: header_length
            size: varint
            type: int
          - id: number_of_columns
            size: varint
            type: int
          - id: columns
            type: column
            repeat: expr
            repeat-expr: this.number_of_columns
          - id: payload
            size: expr
            size-expr: this.payload_length
            type: bytes

        instances:
          column:
            seq:
              - id: type
                size: 1
                type: int
              - id: value
                type:
                  switch-on: type
                  cases:
                    0: {type: int, size: 1}
                    1: {type: int, size: 2}
                    2: {type: int, size: 3}
                    3: {type: int, size: 4}
                    4: {type: int, size: 6}
                    5: {type: int, size: 8}
                    6: {type: float, size: 4}
                    7: {type: float, size: 8}
                    8: {type: str, size: expr, size-expr: varint}
                    9: {type: bytes, size: expr, size-expr: varint}