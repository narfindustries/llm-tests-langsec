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
    if: logical_screen_descriptor.has_global_color_table
    size: logical_screen_descriptor.global_color_table_size
  - id: blocks
    type: block
    repeat: until
    repeat-until: _.block_type == block_type::end_of_file
types:
  header:
    seq:
      - id: magic
        contents: "GIF"
      - id: version
        size: 3
        type: str
        encoding: ASCII
  logical_screen_descriptor:
    seq:
      - id: screen_width
        type: u2
      - id: screen_height
        type: u2
      - id: flags
        type: u1
      - id: bg_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
    instances:
      has_global_color_table:
        value: (flags & 0b10000000) != 0
      color_resolution:
        value: ((flags & 0b01110000) >> 4) + 1
      sort_flag:
        value: (flags & 0b00001000) != 0
      global_color_table_size:
        value: 3 * (1 << ((flags & 0b00000111) + 1))
  global_color_table:
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
      - id: block_data
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
      - id: extension_data
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
        contents: [0x04]
      - id: flags
        type: u1
      - id: delay_time
        type: u2
      - id: transparent_color_index
        type: u1
      - id: terminator
        contents: [0x00]
    instances:
      disposal_method:
        value: (flags & 0b00011100) >> 2
      user_input_flag:
        value: (flags & 0b00000010) != 0
      transparent_color_flag:
        value: (flags & 0b00000001) != 0
  comment_ext:
    seq:
      - id: sub_blocks
        type: sub_blocks
  plain_text_ext:
    seq:
      - id: block_size
        contents: [0x0C]
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
        contents: [0x0B]
      - id: application_identifier
        size: 8
        type: str
        encoding: ASCII
      - id: application_auth_code
        size: 3
      - id: sub_blocks
        type: sub_blocks
  image_block:
    seq:
      - id: image_descriptor
        type: image_descriptor
      - id: local_color_table
        type: local_color_table
        if: image_descriptor.has_local_color_table
        size: image_descriptor.local_color_table_size
      - id: image_data
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
    instances:
      has_local_color_table:
        value: (flags & 0b10000000) != 0
      interlace_flag:
        value: (flags & 0b01000000) != 0
      sort_flag:
        value: (flags & 0b00100000) != 0
      local_color_table_size:
        value: 3 * (1 << ((flags & 0b00000111) + 1))
  local_color_table:
    seq:
      - id: entries
        type: rgb
        repeat: eos
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
  end_block:
    seq: []
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