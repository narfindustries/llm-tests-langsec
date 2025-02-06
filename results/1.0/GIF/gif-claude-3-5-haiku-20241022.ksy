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
    type: color_table
    if: logical_screen_descriptor.packed_screen_descriptor.global_color_table_flag
  - id: blocks
    type: block
    repeat: eos

types:
  header:
    seq:
      - id: signature
        contents: [0x47, 0x49, 0x46]
      - id: version
        type: str
        size: 3
        valid:
          any-of: ['87a', '89a']

  logical_screen_descriptor:
    seq:
      - id: canvas_width
        type: u2
      - id: canvas_height
        type: u2
      - id: packed_screen_descriptor
        type: packed_screen_descriptor
      - id: background_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1

  packed_screen_descriptor:
    seq:
      - id: raw
        type: u1
    instances:
      global_color_table_flag:
        value: (raw & 0b10000000) >> 7
      color_resolution:
        value: (raw & 0b01110000) >> 4
      sort_flag:
        value: (raw & 0b00001000) >> 3
      global_color_table_size:
        value: raw & 0b00000111

  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: 2 ** (global_color_table_size + 1)
    instances:
      global_color_table_size:
        value: _parent.logical_screen_descriptor.packed_screen_descriptor.global_color_table_size

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
    instances:
      content:
        type:
          switch-on: block_type
          cases:
            0x2c: image_descriptor
            0x21: extension
            0x3b: trailer

  trailer:
    seq:
      - id: end_of_file
        contents: [0x3b]

  extension:
    seq:
      - id: extension_type
        type: u1
    instances:
      content:
        type:
          switch-on: extension_type
          cases:
            0xf9: graphic_control_extension
            0xff: application_extension
            0xfe: comment_extension
            0x01: plain_text_extension

  graphic_control_extension:
    seq:
      - id: block_size
        contents: [0x04]
      - id: packed_fields
        type: packed_graphic_control
      - id: delay_time
        type: u2
      - id: transparent_color_index
        type: u1
      - id: block_terminator
        contents: [0x00]

  packed_graphic_control:
    seq:
      - id: raw
        type: u1
    instances:
      disposal_method:
        value: (raw & 0b00011100) >> 2
      user_input_flag:
        value: (raw & 0b00000010) >> 1
      transparent_color_flag:
        value: raw & 0b00000001

  image_descriptor:
    seq:
      - id: left_position
        type: u2
      - id: top_position
        type: u2
      - id: width
        type: u2
      - id: height
        type: u2
      - id: packed_fields
        type: packed_image_descriptor
      - id: local_color_table
        type: color_table
        if: packed_fields.local_color_table_flag
      - id: lzw_minimum_code_size
        type: u1
      - id: image_data
        type: image_data

  packed_image_descriptor:
    seq:
      - id: raw
        type: u1
    instances:
      local_color_table_flag:
        value: (raw & 0b10000000) >> 7
      interlace_flag:
        value: (raw & 0b01000000) >> 6
      sort_flag:
        value: (raw & 0b00100000) >> 5
      local_color_table_size:
        value: raw & 0b00000111

  image_data:
    seq:
      - id: data_blocks
        type: data_block
        repeat: until
        repeat-until: _.block_size == 0

  data_block:
    seq:
      - id: block_size
        type: u1
      - id: data
        size: block_size
        if: block_size > 0

  application_extension:
    seq:
      - id: block_size
        type: u1
      - id: identifier
        type: str
        size: 8
      - id: authentication_code
        type: str
        size: 3
      - id: data_blocks
        type: data_block
        repeat: until
        repeat-until: _.block_size == 0

  comment_extension:
    seq:
      - id: data_blocks
        type: data_block
        repeat: until
        repeat-until: _.block_size == 0

  plain_text_extension:
    seq:
      - id: block_size
        type: u1
      - id: text_grid_left_position
        type: u2
      - id: text_grid_top_position
        type: u2
      - id: text_grid_width
        type: u2
      - id: text_grid_height
        type: u2
      - id: cell_width
        type: u1
      - id: cell_height
        type: u1
      - id: foreground_color_index
        type: u1
      - id: background_color_index
        type: u1
      - id: data_blocks
        type: data_block
        repeat: until
        repeat-until: _.block_size == 0