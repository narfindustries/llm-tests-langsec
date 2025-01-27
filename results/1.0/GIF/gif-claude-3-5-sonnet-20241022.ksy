meta:
  id: gif
  file-extension: gif
  endian: le

seq:
  - id: header
    type: header
  - id: logical_screen
    type: logical_screen
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

  logical_screen:
    seq:
      - id: image_width
        type: u2
      - id: image_height
        type: u2
      - id: flags
        type: u1
      - id: bg_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
    instances:
      has_color_table:
        value: (flags & 0b10000000) >> 7
      color_table_size:
        value: 2 << ((flags & 0b00000111) - 1) if has_color_table != 0 else 0

  block:
    seq:
      - id: block_type
        type: u1
        enum: block_type
      - id: data
        type:
          switch-on: block_type
          cases:
            'block_type::extension': extension_block
            'block_type::image': image_block
    enums:
      block_type:
        0x21: extension
        0x2c: image
        0x3b: end_of_file

  extension_block:
    seq:
      - id: extension_type
        type: u1
        enum: extension_type
      - id: data_blocks
        type: data_block
        repeat: until
        repeat-until: _.block_size == 0
    enums:
      extension_type:
        0xf9: graphic_control
        0xfe: comment
        0xff: application

  image_block:
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
        if: has_local_color_table
      - id: image_data
        type: image_data
    instances:
      has_local_color_table:
        value: (flags & 0b10000000) >> 7
      local_color_table_size:
        value: 2 << ((flags & 0b00000111) - 1) if has_local_color_table != 0 else 0

  color_table:
    seq:
      - id: entries
        type: color
        repeat: expr
        repeat-expr: _parent.local_color_table_size

  color:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

  data_block:
    seq:
      - id: block_size
        type: u1
      - id: data
        size: block_size
        if: block_size != 0

  image_data:
    seq:
      - id: lzw_min_code_size
        type: u1
      - id: data_blocks
        type: data_block
        repeat: until
        repeat-until: _.block_size == 0