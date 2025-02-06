meta:
  id: gif
  file-extension: gif
  endian: le
  title: Graphics Interchange Format (GIF)
  license: CC0-1.0
doc: |
  The Graphics Interchange Format (GIF) is a bitmap image format that was developed
  by a team at the online services provider CompuServe led by American computer scientist
  Steve Wilhite on June 15, 1987.

seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: color_table
    if: logical_screen_descriptor.gct_flag
    repeat: expr
    repeat-expr: 1 << (logical_screen_descriptor.gct_size + 1)
  - id: blocks
    type: block
    repeat: eos

types:
  header:
    seq:
      - id: magic
        contents: "GIF"
      - id: version
        contents: "89a"

  logical_screen_descriptor:
    seq:
      - id: canvas_width
        type: u2
      - id: canvas_height
        type: u2
      - id: packed_fields
        type: u1
      - id: bg_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
    instances:
      gct_flag:
        value: (packed_fields & 0b10000000) != 0
      color_resolution:
        value: ((packed_fields & 0b01110000) >> 4) + 1
      sort_flag:
        value: (packed_fields & 0b00001000) != 0
      gct_size:
        value: packed_fields & 0b00000111

  color_table:
    seq:
      - id: colors
        type: rgb
        repeat: expr
        repeat-expr: 1 << (_parent.gct_size + 1)

  rgb:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

  block:
    seq:
      - id: block_type
        type: u1
      - id: body
        type:
          switch-on: block_type
          cases:
            0x2c: image_block
            0x21: extension
            0x3b: end_block

  image_block:
    seq:
      - id: image_left
        type: u2
      - id: image_top
        type: u2
      - id: image_width
        type: u2
      - id: image_height
        type: u2
      - id: packed_fields
        type: u1
      - id: lct
        type: color_table
        if: (packed_fields & 0b10000000) != 0
      - id: lzw_min_code_size
        type: u1
      - id: image_data
        type: data_sub_blocks

  data_sub_blocks:
    seq:
      - id: len_data
        type: u1
      - id: data
        size: len_data
      - id: next_block
        type: data_sub_blocks
        if: len_data != 0

  extension:
    seq:
      - id: label
        type: u1
      - id: body
        type:
          switch-on: label
          cases:
            0xf9: graphic_control
            0xfe: comment_extension
            0x01: plain_text_extension
            0xff: application_extension

  graphic_control:
    seq:
      - id: block_size
        contents: [0x04]
      - id: packed_fields
        type: u1
      - id: delay_time
        type: u2
      - id: transparent_color_index
        type: u1
      - id: terminator
        type: u1

  comment_extension:
    seq:
      - id: block_size
        type: u1
      - id: comment_data
        type: str
        encoding: ASCII
        size: block_size

  plain_text_extension:
    seq:
      - id: block_size
        contents: [0x0c]
      - id: text_grid_left
        type: u2
      - id: text_grid_top
        type: u2
      - id: text_grid_width
        type: u2
      - id: text_grid_height
        type: u2
      - id: char_width
        type: u1
      - id: char_height
        type: u1
      - id: text_fg_color_index
        type: u1
      - id: text_bg_color_index
        type: u1
      - id: plain_text_data
        type: data_sub_blocks

  application_extension:
    seq:
      - id: block_size
        contents: [0x0b]
      - id: app_identifier
        type: str
        encoding: ASCII
        size: 8
      - id: app_code
        type: str
        encoding: ASCII
        size: 3
      - id: app_data
        type: data_sub_blocks

  end_block:
    seq: []