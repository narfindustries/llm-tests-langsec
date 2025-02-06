meta:
  id: gif
  file-extension: gif
  endian: le
doc: |
  GIF (Graphics Interchange Format) is an image format developed by CompuServe in 1987.
  This parser covers the structure of version 89a.
seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: rgb
    repeat: expr
    repeat-expr: "logical_screen_descriptor.global_color_table_flag == 1 ? (1 << (logical_screen_descriptor.size_of_global_color_table + 1)) : 0"
  - id: blocks
    type: block
    repeat: eos

types:
  header:
    seq:
      - id: signature
        type: str
        size: 3
        encoding: ASCII
      - id: version
        type: str
        size: 3
        encoding: ASCII

  logical_screen_descriptor:
    seq:
      - id: canvas_width
        type: u2
      - id: canvas_height
        type: u2
      - id: packed_fields
        type: u1
      - id: background_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
    instances:
      global_color_table_flag:
        value: '(packed_fields & 0b10000000) >> 7'
      color_resolution:
        value: '((packed_fields & 0b01110000) >> 4) + 1'
      sort_flag:
        value: '(packed_fields & 0b00001000) >> 3'
      size_of_global_color_table:
        value: 'packed_fields & 0b00000111'

  rgb:
    seq:
      - id: r
        type: u1
      - id: g
        type: u1
      - id: b
        type: u1

  block:
    seq:
      - id: block_type
        type: u1
      - id: block_head
        type: block_head
      - id: block_content
        type: block_content
        size-eos: true

  block_head:
    seq:
      - id: block_size
        type: u1

  block_content:
    seq:
      - id: data
        size: 'block_head.block_size'
        type: u1

  image_descriptor:
    seq:
      - id: left
        type: u2
      - id: top
        type: u2
      - id: width
        type: u2
      - id: height
        type: u2
      - id: packed_fields
        type: u1
      - id: lzw_min_code_size
        type: u1
      - id: image_data
        type: sub_blocks

  sub_blocks:
    seq:
      - id: block_size
        type: u1
      - id: block_data
        type: u1
        repeat: expr
        repeat-expr: block_size
      - id: terminator
        type: u1
        repeat-until: _ == 0
        doc: Terminator is a single null byte indicating the end of data blocks.