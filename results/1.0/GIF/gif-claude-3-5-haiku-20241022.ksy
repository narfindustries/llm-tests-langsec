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
    size: logical_screen_descriptor.global_color_table_size * 3
  - id: blocks
    type: block
    repeat: until
    repeat-until: _.block_type == block_type::trailer
types:
  header:
    seq:
      - id: signature
        contents: ['GIF']
      - id: version
        type: str
        size: 3
        encoding: ascii
  logical_screen_descriptor:
    seq:
      - id: canvas_width
        type: u2
      - id: canvas_height
        type: u2
      - id: flags
        type: u1
      - id: bg_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
    instances:
      global_color_table_flag:
        value: '(flags & 0b10000000) != 0'
      global_color_table_size:
        value: 'flags & 0b00000111'
  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: 256
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
    instances:
      extension:
        type: extension_block
        if: block_type == block_type::extension
      image_descriptor:
        type: image_descriptor
        if: block_type == block_type::image_descriptor
  extension_block:
    seq:
      - id: extension_type
        type: u1
        enum: extension_type
      - id: data
        size-eos: true
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
      - id: local_color_table
        type: color_table
        if: local_color_table_flag
        size: local_color_table_size * 3
      - id: lzw_min_code_size
        type: u1
      - id: image_data
        type: image_data
    instances:
      local_color_table_flag:
        value: '(flags & 0b10000000) != 0'
      local_color_table_size:
        value: 'flags & 0b00000111'
  image_data:
    seq:
      - id: blocks
        type: data_sub_block
        repeat: until
        repeat-until: _.block_terminator == 0
  data_sub_block:
    seq:
      - id: block_size
        type: u1
      - id: block_terminator
        type: u1
      - id: block_data
        size: block_size
        if: block_size > 0
enums:
  block_type:
    0x21: extension
    0x2c: image_descriptor
    0x3b: trailer
  extension_type:
    0xf9: graphic_control_extension
    0xff: application_extension
    0x01: plain_text_extension
    0xfe: comment_extension