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
    repeat: until
    repeat-until: _.type == "trailer"
types:
  header:
    seq:
      - id: signature
        contents: "GIF"
      - id: version
        contents: ["87a", "89a"]
  logical_screen_descriptor:
    seq:
      - id: logical_screen_width
        type: u2
      - id: logical_screen_height
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
        repeat-expr: size_of_global_color_table
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
      - id: type
        type: u1
      - id: content
        type:
          switch-on: type
          cases:
            0x2C: image_descriptor
            0x21: extension
            0x3B: trailer
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
        if: (packed_fields & 0x80) >> 7 == 1
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
      - id: sub_blocks
        type: sub_block
        repeat: until
        repeat-until: _.size == 0
  sub_block:
    seq:
      - id: size
        type: u1
      - id: data
        size: size
  extension:
    seq:
      - id: label
        type: u1
      - id: content
        type:
          switch-on: label
          cases:
            0xF9: graphic_control_extension
            0x01: plain_text_extension
            0xFF: application_extension
            0xFE: comment_extension
  graphic_control_extension:
    seq:
      - id: block_size
        type: u1
      - id: packed_fields
        type: u1
      - id: delay_time
        type: u2
      - id: transparent_color_index
        type: u1
      - id: terminator
        type: u1
    instances:
      disposal_method:
        value: (packed_fields & 0x1C) >> 2
      user_input_flag:
        value: (packed_fields & 0x02) >> 1
      transparency_flag:
        value: packed_fields & 0x01
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
      - id: character_cell_width
        type: u1
      - id: character_cell_height
        type: u1
      - id: text_foreground_color_index
        type: u1
      - id: text_background_color_index
        type: u1
      - id: sub_blocks
        type: sub_block
        repeat: until
        repeat-until: _.size == 0
  application_extension:
    seq:
      - id: block_size
        type: u1
      - id: application_identifier
        size: 8
      - id: application_authentication_code
        size: 3
      - id: sub_blocks
        type: sub_block
        repeat: until
        repeat-until: _.size == 0
  comment_extension:
    seq:
      - id: sub_blocks
        type: sub_block
        repeat: until
        repeat-until: _.size == 0
  trailer:
    seq:
      - id: value
        type: u1
        valid: value == 0x3B