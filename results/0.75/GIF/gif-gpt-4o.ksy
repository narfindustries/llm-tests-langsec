meta:
  id: gif
  title: GIF File Format
  file-extension: gif
  endian: le
seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: color_table
    if: header.logical_screen_descriptor.has_global_color_table
    size: 3 * (1 << (header.logical_screen_descriptor.size_of_global_color_table + 1))
  - id: blocks
    type: block
    repeat: until
    repeat-until: blocks[-1].is_trailer
types:
  header:
    seq:
      - id: signature
        contents: "GIF"
      - id: version
        size: 3
        type: str
  logical_screen_descriptor:
    seq:
      - id: logical_screen_width
        type: u2
      - id: logical_screen_height
        type: u2
      - id: packed_fields
        type: b1
      - id: background_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
    instances:
      has_global_color_table:
        value: '(packed_fields & 0x80) != 0'
      color_resolution:
        value: '(packed_fields >> 4) & 0x07'
      sort_flag:
        value: '(packed_fields & 0x08) != 0'
      size_of_global_color_table:
        value: 'packed_fields & 0x07'
  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: '1 << (_root.header.logical_screen_descriptor.size_of_global_color_table + 1)'
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
      - id: sentinel
        type: u1
      - id: data
        type:
          switch-on: sentinel
          cases:
            0x21: extension
            0x2C: image_descriptor
            0x3B: null # trailer
    instances:
      is_trailer:
        value: 'sentinel == 0x3B'
  extension:
    seq:
      - id: label
        type: u1
      - id: body
        size: 'label == 0xF9 ? 4 : -1'
        type:
          switch-on: label
          cases:
            0xF9: graphic_control_extension
            0xFE: comment_extension
            0xFF: application_extension
            0x01: plain_text_extension
      - id: block_terminator
        type: u1
        assert: 'block_terminator == 0x00'
  graphic_control_extension:
    seq:
      - id: block_size
        type: u1
        assert: 'block_size == 4'
      - id: packed_fields
        type: b1
      - id: delay_time
        type: u2
      - id: transparent_color_index
        type: u1
    instances:
      reserved:
        value: '(packed_fields >> 5) & 0x07'
      disposal_method:
        value: '(packed_fields >> 2) & 0x07'
      user_input_flag:
        value: '(packed_fields & 0x02) != 0'
      transparent_color_flag:
        value: '(packed_fields & 0x01) != 0'
  comment_extension:
    seq:
      - id: comments
        type: sub_blocks
  application_extension:
    seq:
      - id: block_size
        type: u1
        assert: 'block_size == 11'
      - id: application_identifier
        type: str
        size: 8
      - id: application_auth_code
        type: str
        size: 3
      - id: application_data
        type: sub_blocks
  plain_text_extension:
    seq:
      - id: block_size
        type: u1
        assert: 'block_size == 12'
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
        type: b1
      - id: local_color_table
        type: color_table
        if: has_local_color_table
        size: 3 * (1 << (size_of_local_color_table + 1))
      - id: lzw_minimum_code_size
        type: u1
      - id: image_data
        type: sub_blocks
    instances:
      has_local_color_table:
        value: '(packed_fields & 0x80) != 0'
      interlace_flag:
        value: '(packed_fields & 0x40) != 0'
      sort_flag:
        value: '(packed_fields & 0x20) != 0'
      reserved:
        value: '(packed_fields >> 3) & 0x03'
      size_of_local_color_table:
        value: 'packed_fields & 0x07'
  sub_blocks:
    seq:
      - id: sub_block
        type: data_sub_block
        repeat: until
        repeat-until: sub_block.size == 0
  data_sub_block:
    seq:
      - id: size
        type: u1
      - id: data
        size: 'size'