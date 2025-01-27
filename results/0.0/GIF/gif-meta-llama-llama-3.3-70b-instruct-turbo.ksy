meta:
  id: gif
  file-extension: gif
  endian: le
seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: global_color_table
    if: logical_screen_descriptor.global_color_table_flag
  - id: images
    type: image
    repeat: until
    until: _ == 0x3b
types:
  header:
    seq:
      - id: magic
        content: 'GIF'
      - id: version
        size: 3
  logical_screen_descriptor:
    seq:
      - id: width
        type: u2
      - id: height
        type: u2
      - id: flags
        type: u1
      - id: background_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
    instances:
      global_color_table_flag:
        value: flags & 0x80
  global_color_table:
    seq:
      - id: colors
        type: color_table_entry
        repeat: expr
        repeat-expr: 2 << (logical_screen_descriptor.flags & 0x7)
  color_table_entry:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1
  image:
    seq:
      - id: descriptor
        type: image_descriptor
      - id: data
        type: image_data
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
      - id: flags
        type: u1
  image_data:
    seq:
      - id: min_code_size
        type: u1
      - id: sub_blocks
        type: sub_block
        repeat: until
        until: _ == 0x00
  sub_block:
    seq:
      - id: length
        type: u1
      - id: data
        size: length