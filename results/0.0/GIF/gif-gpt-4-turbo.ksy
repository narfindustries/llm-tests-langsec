meta:
  id: gif
  file-extension: gif
  endian: le
  title: Graphics Interchange Format (GIF)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  The Graphics Interchange Format (GIF) is a bitmap image format that was developed
  by a team at the online services provider CompuServe led by American computer scientist
  Steve Wilhite. It was first released on 15 June 1987.

seq:
  - id: header
    type: header

  - id: logical_screen_descriptor
    type: logical_screen_descriptor

  - id: global_color_table
    type: color_table
    if: logical_screen_descriptor.has_global_color_table
    size: logical_screen_descriptor.global_color_table_size

  - id: blocks
    type: block
    repeat: eos

types:
  header:
    seq:
      - id: signature
        size: 3
        contents: 'GIF'
      - id: version
        size: 3
        enum: version
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
        value: (flags & 0x80) != 0
      color_resolution:
        value: ((flags >> 4) & 0x07) + 1
      sort_flag:
        value: (flags & 0x08) != 0
      global_color_table_size:
        value: (2 ** ((flags & 0x07) + 1)) * 3

  color_table:
    seq:
      - id: colors
        type: rgb
        repeat: expr
        repeat-expr: _parent.global_color_table_size / 3

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
        enum: block_type
    instances:
      body:
        io: _io
        pos: _io.pos
        type:
          switch-on: block_type
          cases:
            'block_type::image_descriptor': image_block
            'block_type::extension': extension_block
            'block_type::end_of_file': eos_marker

  image_block:
    seq:
      - id: image_descriptor
        type: image_descriptor
      - id: local_color_table
        type: color_table
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
        value: (flags & 0x80) != 0
      interlace:
        value: (flags & 0x40) != 0
      sort:
        value: (flags & 0x20) != 0
      local_color_table_size:
        value: (2 ** ((flags & 0x07) + 1)) * 3

  image_data:
    seq:
      - id: lzw_min_code_size
        type: u1
      - id: data_blocks
        type: data_block
        repeat: eos

  data_block:
    seq:
      - id: block_size
        type: u1
      - id: data
        size: block_size

  extension_block:
    seq:
      - id: extension_type
        type: u1
        enum: extension_type
      - id: block_size
        type: u1
      - id: data
        size: block_size

  eos_marker:
    seq: []

enums:
  version:
    87a: '87a'
    89a: '89a'

  block_type:
    0x2c: image_descriptor
    0x21: extension
    0x3b: end_of_file

  extension_type:
    0xf9: graphic_control
    0xfe: comment
    0x01: plain_text
    0xff: application