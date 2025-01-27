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
    repeat-until: _.block_type == block_type.end_of_file
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
      - id: block_type
        type: u1
      - id: block_data
        type:
          switch-on: block_type
          cases:
            0x21: extension_block
            0x2C: image_descriptor
            0x3B: end_of_file_block
  extension_block:
    seq:
      - id: extension_type
        type: u1
      - id: extension_data
        type:
          switch-on: extension_type
          cases:
            0xF9: graphic_control_extension
            0x01: plain_text_extension
            0xFF: application_extension
            0xFE: comment_extension
  graphic_control_extension:
    seq:
      - id: block_size
        type: u1
        contents: 4
      - id: packed_fields
        type: u1
      - id: delay_time
        type: u2
      - id: transparent_color_index
        type: u1
      - id: terminator
        type: u1
        contents: 0
  plain_text_extension:
    seq:
      - id: block_size
        type: u1
        contents: 12
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
      - id: text_fg_color_index
        type: u1
      - id: text_bg_color_index
        type: u1
      - id: plain_text_data
        type: subblocks
  application_extension:
    seq:
      - id: block_size
        type: u1
        contents: 11
      - id: application_identifier
        type: str
        size: 8
      - id: application_authentication_code
        type: str
        size: 3
      - id: application_data
        type: subblocks
  comment_extension:
    seq:
      - id: comment_data
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
      - id: local_color_table
        type: color_table
        if: (packed_fields & 0x80) != 0
        size: 2 << (packed_fields & 0x07)
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
        repeat-until: size == 0
  end_of_file_block:
    seq:
      - id: terminator
        type: u1
        contents: 0
enums:
  block_type:
    0x21: extension
    0x2C: image
    0x3B: end_of_file