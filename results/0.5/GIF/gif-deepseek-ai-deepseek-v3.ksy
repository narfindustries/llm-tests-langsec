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
    if: logical_screen_descriptor.global_color_table_flag == 1
  - id: blocks
    type: block
    repeat: eos
types:
  header:
    seq:
      - id: signature
        contents: "GIF"
      - id: version
        size: 3
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
        value: (packed_fields & 0x80) >> 7
      color_resolution:
        value: ((packed_fields & 0x70) >> 4) + 1
      sort_flag:
        value: (packed_fields & 0x08) >> 3
      size_of_global_color_table:
        value: 2 << (packed_fields & 0x07)
  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: _root.logical_screen_descriptor.size_of_global_color_table
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
      - id: block_data
        type:
          switch-on: block_type
          cases:
            0x21: extension_block
            0x2C: image_descriptor
            0x3B: trailer_block
  extension_block:
    seq:
      - id: extension_label
        type: u1
      - id: extension_data
        type:
          switch-on: extension_label
          cases:
            0xF9: graphic_control_extension
            0x01: plain_text_extension
            0xFF: application_extension
            0xFE: comment_extension
  graphic_control_extension:
    seq:
      - id: block_size
        type: u1
        valid: block_size == 4
      - id: packed_fields
        type: u1
      - id: delay_time
        type: u2
      - id: transparent_color_index
        type: u1
      - id: block_terminator
        type: u1
        valid: block_terminator == 0
    instances:
      disposal_method:
        value: (packed_fields & 0x1C) >> 2
      user_input_flag:
        value: (packed_fields & 0x02) >> 1
      transparent_color_flag:
        value: packed_fields & 0x01
  plain_text_extension:
    seq:
      - id: block_size
        type: u1
        valid: block_size == 12
      - id: text_grid_left_position
        type: u2
      - id: text_grid_top_position
        type: u2
      - id: text_grid_width
        type: u2
      - id: text_grid_height
        type: u2
      - id: character_cell_width
        type: u1
      - id: character_cell_height
        type: u1
      - id: text_foreground_color_index
        type: u1
      - id: text_background_color_index
        type: u1
      - id: plain_text_data
        type: sub_blocks
      - id: block_terminator
        type: u1
        valid: block_terminator == 0
  application_extension:
    seq:
      - id: block_size
        type: u1
        valid: block_size == 11
      - id: application_identifier
        size: 8
      - id: application_authentication_code
        size: 3
      - id: application_data
        type: sub_blocks
      - id: block_terminator
        type: u1
        valid: block_terminator == 0
  comment_extension:
    seq:
      - id: comment_data
        type: sub_blocks
      - id: block_terminator
        type: u1
        valid: block_terminator == 0
  image_descriptor:
    seq:
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
      - id: local_color_table
        type: color_table
        if: local_color_table_flag == 1
      - id: image_data
        type: image_data
    instances:
      local_color_table_flag:
        value: (packed_fields & 0x80) >> 7
      interlace_flag:
        value: (packed_fields & 0x40) >> 6
      sort_flag:
        value: (packed_fields & 0x20) >> 5
      size_of_local_color_table:
        value: 2 << (packed_fields & 0x07)
  image_data:
    seq:
      - id: lzw_minimum_code_size
        type: u1
      - id: compressed_image_data
        type: sub_blocks
  sub_blocks:
    seq:
      - id: len_sub_block_data
        type: u1
      - id: sub_block_data
        size: len_sub_block_data
        repeat: until
        repeat-until: len_sub_block_data == 0
  trailer_block:
    seq:
      - id: trailer
        type: u1
        valid: trailer == 0x3B