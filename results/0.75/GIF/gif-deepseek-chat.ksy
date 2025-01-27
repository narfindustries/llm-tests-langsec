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
    if: logical_screen_descriptor.global_color_table_flag
    size: logical_screen_descriptor.global_color_table_size
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
        value: (packed_fields & 0x80) != 0
      color_resolution:
        value: ((packed_fields & 0x70) >> 4) + 1
      global_color_table_size:
        value: 2 << (packed_fields & 0x07)
  color_table:
    seq:
      - id: entries
        type: color_table_entry
        repeat: expr
        repeat-expr: _root.logical_screen_descriptor.global_color_table_size
  color_table_entry:
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
      - id: body
        type:
          switch-on: type
          cases:
            0x21: extension
            0x2C: image_descriptor
            0x3B: trailer
  extension:
    seq:
      - id: label
        type: u1
      - id: body
        type:
          switch-on: label
          cases:
            0x01: plain_text_extension
            0xF9: graphic_control_extension
            0xFE: comment_extension
            0xFF: application_extension
  plain_text_extension:
    seq:
      - id: block_size
        type: u1
      - id: text_grid_left
        type: u2
      - id: text_grid_top
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
        type: subblocks
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
    instances:
      disposal_method:
        value: (packed_fields & 0x1C) >> 2
      user_input_flag:
        value: (packed_fields & 0x02) != 0
      transparent_color_flag:
        value: (packed_fields & 0x01) != 0
  comment_extension:
    seq:
      - id: comment_data
        type: subblocks
  application_extension:
    seq:
      - id: block_size
        type: u1
      - id: application_identifier
        size: 8
      - id: application_authentication_code
        size: 3
      - id: application_data
        type: subblocks
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
    instances:
      local_color_table_flag:
        value: (packed_fields & 0x80) != 0
      interlace_flag:
        value: (packed_fields & 0x40) != 0
      sort_flag:
        value: (packed_fields & 0x20) != 0
      local_color_table_size:
        value: 2 << (packed_fields & 0x07)
    seq:
      - id: local_color_table
        type: color_table
        if: local_color_table_flag
        size: local_color_table_size
      - id: image_data
        type: image_data
  image_data:
    seq:
      - id: lzw_minimum_code_size
        type: u1
      - id: subblocks
        type: subblocks
  subblocks:
    seq:
      - id: size
        type: u1
      - id: data
        size: size
        repeat: until
        repeat-until: _ == 0
  trailer:
    seq:
      - id: value
        contents: "0x3B"