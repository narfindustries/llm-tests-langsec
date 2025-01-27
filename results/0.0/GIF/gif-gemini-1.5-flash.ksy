meta:
  id: gif-gemini-1
  title: GIF (Gemini 1.5 Flash)
  homepage: https://kaitai.io/
  file-extension: gif
  experimental: true

types:
  header:
    seq:
      - id: signature
        type: str
        size: 3
        enum:
          - 0: GIF
      - id: version
        type: str
        size: 3

  screen_descriptor:
    seq:
      - id: width
        type: u2
      - id: height
        type: u2
      - id: packed_fields
        type: u1
        repeat: expr
          - 0: global_color_table_flag
          - 1: color_resolution
          - 2: sort_flag
          - 3: size_of_global_color_table
      - id: background_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1

  global_color_table:
    seq:
      - id: entries
        type: color_table_entry
        repeat: expr
          - this.parent.screen_descriptor.packed_fields & 7 + 1

  color_table_entry:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

  image_descriptor:
    seq:
      - id: image_separator
        type: u1
        enum:
          - 0: 44a
      - id: image_left_position
        type: u2
      - id: image_top_position
        type: u2
      - id: image_width
        type: u2
      - id: image_height
        type: u2
      - id: packed_fields
        type: u1
        repeat: expr
          - 0: local_color_table_flag
          - 1: interlace_flag
          - 2: sort_flag
          - 3: size_of_local_color_table
      - id: local_color_table
        type: global_color_table
        if: this.packed_fields & 0x80 != 0

  image_data:
    seq:
      - id: lzw_minimum_code_size
        type: u1
      - id: data_blocks
        type: data_block
        repeat: until
          - this.last.block_terminator == 0

  data_block:
    seq:
      - id: block_size
        type: u1
      - id: block_data
        type: u1
        repeat: expr
          - this.block_size
      - id: block_terminator
        type: u1

  trailer:
    seq:
      - id: trailer
        type: u1
        enum:
          - 0: 3b


seq:
  - id: header
    type: header
  - id: screen_descriptor
    type: screen_descriptor
  - id: global_color_table
    type: global_color_table
    if: this.parent.screen_descriptor.packed_fields & 0x80 != 0
  - id: image_descriptor
    type: image_descriptor
    repeat: until
      - this.last.image_separator == 0x3b
  - id: image_data
    type: image_data
    repeat: expr
      - this.parent.image_descriptor.length
  - id: trailer
    type: trailer

