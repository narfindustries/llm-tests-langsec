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
    if: logical_screen_descriptor.has_global_color_table
    size: logical_screen_descriptor.global_color_table_size * 3
  - id: blocks
    type: block
    repeat: until
    repeat-until: _.block_type == block_type::end_of_file
types:
  header:
    seq:
      - id: magic
        contents: 'GIF'
      - id: version
        type: str
        size: 3
        encoding: ASCII
  logical_screen_descriptor:
    seq:
      - id: screen_width
        type: u2
      - id: screen_height
        type: u2
      - id: packed_fields
        type: u1
      - id: bg_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
    instances:
      has_global_color_table:
        value: (packed_fields & 0b10000000) != 0
      color_resolution:
        value: (packed_fields & 0b01110000) >> 4
      sort_flag:
        value: (packed_fields & 0b00001000) != 0
      global_color_table_size:
        value: 1 << ((packed_fields & 0b00000111) + 1)
  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: eos
  rgb:
    seq:
      - id: r
        type: u1
      - id: g
        type: u1
      - id: b
        type: u1
  block:
    seq:
      - id: block_type
        type: u1
        enum: block_type
      - id: body
        type:
          switch-on: block_type
          cases:
            'block_type::extension': extension_block
            'block_type::image': image_block
            'block_type::end_of_file': end_block
  extension_block:
    seq:
      - id: extension_type
        type: u1
        enum: extension_type
      - id: body
        type:
          switch-on: extension_type
          cases:
            'extension_type::graphics_control': graphics_control_ext
            'extension_type::comment': comment_ext
            'extension_type::plain_text': plain_text_ext
            'extension_type::application': application_ext
  graphics_control_ext:
    seq:
      - id: block_size
        type: u1
        valid: 4
      - id: packed_fields
        type: u1
      - id: delay_time
        type: u2
      - id: transparent_color_index
        type: u1
      - id: terminator
        type: u1
        valid: 0
    instances:
      disposal_method:
        value: (packed_fields & 0b00011100) >> 2
      user_input_flag:
        value: (packed_fields & 0b00000010) != 0
      transparent_color_flag:
        value: (packed_fields & 0b00000001) != 0
  comment_ext:
    seq:
      - id: sub_blocks
        type: sub_blocks
  plain_text_ext:
    seq:
      - id: block_size
        type: u1
        valid: 12
      - id: grid_left
        type: u2
      - id: grid_top
        type: u2
      - id: grid_width
        type: u2
      - id: grid_height
        type: u2
      - id: cell_width
        type: u1
      - id: cell_height
        type: u1
      - id: fg_color_index
        type: u1
      - id: bg_color_index
        type: u1
      - id: sub_blocks
        type: sub_blocks
  application_ext:
    seq:
      - id: block_size
        type: u1
        valid: 11
      - id: application_identifier
        type: str
        size: 8
        encoding: ASCII
      - id: application_auth_code
        size: 3
      - id: sub_blocks
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
        type: color_table
        if: has_local_color_table
        size: local_color_table_size * 3
      - id: image_data
        type: image_data
    instances:
      has_local_color_table:
        value: (packed_fields & 0b10000000) != 0
      is_interlaced:
        value: (packed_fields & 0b01000000) != 0
      is_sorted:
        value: (packed_fields & 0b00100000) != 0
      local_color_table_size:
        value: 1 << ((packed_fields & 0b00000111) + 1)
  image_data:
    seq:
      - id: lzw_min_code_size
        type: u1
      - id: sub_blocks
        type: sub_blocks
  sub_blocks:
    seq:
      - id: entries
        type: sub_block
        repeat: until
        repeat-until: _.block_size == 0
  sub_block:
    seq:
      - id: block_size
        type: u1
      - id: data
        size: block_size
  end_block: {}
enums:
  block_type:
    0x21: extension
    0x2c: image
    0x3b: end_of_file
  extension_type:
    0xf9: graphics_control
    0xfe: comment
    0x01: plain_text
    0xff: application