meta:
  id: gif
  file-extension: gif
  endian: le
  license: CC0-1.0
seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: global_color_table
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
        contents: "89a"
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
        value: 'packed_fields >> 7'
      color_resolution:
        value: '(packed_fields >> 4) & 0x07'
      sort_flag:
        value: '(packed_fields >> 3) & 0x01'
      global_color_table_size:
        value: '(1 << ((packed_fields & 0x07) + 1))'
  global_color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: _root.logical_screen_descriptor.global_color_table_size
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
            0x2C: image_block
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
            0xFE: comment_extension
            0x01: plain_text_extension
            0xFF: application_extension
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
      - id: block_terminator
        type: u1
    instances:
      disposal_method:
        value: '(packed_fields >> 2) & 0x07'
      user_input_flag:
        value: '(packed_fields >> 1) & 0x01'
      transparent_color_flag:
        value: 'packed_fields & 0x01'
  comment_extension:
    seq:
      - id: comment_data
        type: sub_blocks
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
      - id: text_color_index
        type: u1
      - id: background_color_index
        type: u1
      - id: plain_text_data
        type: sub_blocks
  application_extension:
    seq:
      - id: block_size
        type: u1
      - id: application_identifier
        size: 8
      - id: application_authentication_code
        size: 3
      - id: application_data
        type: sub_blocks
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
      - id: local_color_table
        type: local_color_table
        if: 'packed_fields >> 7 == 1'
      - id: image_data
        type: image_data
    instances:
      local_color_table_flag:
        value: 'packed_fields >> 7'
      interlace_flag:
        value: '(packed_fields >> 6) & 0x01'
      sort_flag:
        value: '(packed_fields >> 5) & 0x01'
      local_color_table_size:
        value: '(1 << ((packed_fields & 0x07) + 1))'
  local_color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: _parent.local_color_table_size
  image_data:
    seq:
      - id: lzw_minimum_code_size
        type: u1
      - id: lzw_compressed_data
        type: sub_blocks
  sub_blocks:
    seq:
      - id: blocks
        type: sub_block
        repeat: until
        repeat-until: '_.size == 0'
  sub_block:
    seq:
      - id: size
        type: u1
      - id: data
        size: size
  trailer_block:
    seq:
      - id: trailer
        type: u1
        valid: 'trailer == 0x3B'