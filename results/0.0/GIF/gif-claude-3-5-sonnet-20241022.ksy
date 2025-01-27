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
        value: (flags & 0b10000000) != 0
      color_table_size:
        value: 1 << ((flags & 0b00000111) + 1) if has_color_table else 0

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
      - id: data
        type:
          switch-on: extension_type
          cases:
            'extension_type::graphics_control': graphics_control_ext
            'extension_type::comment': comment_ext
            'extension_type::application': application_ext
            _: skip_blocks
    enums:
      extension_type:
        0xf9: graphics_control
        0xfe: comment
        0xff: application

  graphics_control_ext:
    seq:
      - id: block_size
        contents: [0x04]
      - id: flags
        type: u1
      - id: delay_time
        type: u2
      - id: transparent_idx
        type: u1
      - id: terminator
        contents: [0x00]

  comment_ext:
    seq:
      - id: data
        type: data_blocks

  application_ext:
    seq:
      - id: block_size
        contents: [0x0b]
      - id: application_id
        type: str
        size: 8
        encoding: ASCII
      - id: auth_code
        size: 3
      - id: data
        type: data_blocks

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
        value: (flags & 0b10000000) != 0
      local_color_table_size:
        value: 1 << ((flags & 0b00000111) + 1) if has_local_color_table else 0

  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: _parent.local_color_table_size

  rgb:
    seq:
      - id: r
        type: u1
      - id: g
        type: u1
      - id: b
        type: u1

  image_data:
    seq:
      - id: lzw_min_code_size
        type: u1
      - id: blocks
        type: data_blocks

  data_blocks:
    seq:
      - id: blocks
        type: data_block
        repeat: until
        repeat-until: _.block_size == 0

  data_block:
    seq:
      - id: block_size
        type: u1
      - id: data
        size: block_size

  skip_blocks:
    seq:
      - id: blocks
        type: data_blocks