meta:
  id: gif
  file-extension: gif
  endian: le
  title: "Graphics Interchange Format (GIF)"
  license: CC0-1.0
  ks-version: 0.9

doc: |
  The Graphics Interchange Format is a bitmap image format that was developed
  by a team at the online services provider CompuServe led by American computer scientist
  Steve Wilhite on June 15, 1987. It has since come into widespread usage on the World Wide Web
  due to its wide support and portability between applications and operating systems.

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
        contents: "GIF"
      - id: version
        contents: ['87a', '89a']

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
        value: flags & 0b10000000 != 0
      color_resolution:
        value: ((flags & 0b01110000) >> 4) + 1
      sort_flag:
        value: (flags & 0b00001000) != 0
      global_color_table_size:
        value: (2 ** ((flags & 0b00000111) + 1)) * 3

  color_table:
    seq:
      - id: colors
        type: rgb
        repeat: expr
        repeat-expr: _parent.global_color_table_size / 3

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
    instances:
      body:
        io: _io
        pos: _io.pos
        type:
          switch-on: block_type
          cases:
            0x2c: image_block
            0x21: extension_block
        eos-error: false

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
      - id: flags
        type: u1
      - id: local_color_table
        type: color_table
        if: flags & 0b10000000 != 0
        size: (2 ** ((flags & 0b00000111) + 1)) * 3
      - id: lzw_min_code_size
        type: u1
      - id: image_data
        type: image_data_block

  extension_block:
    seq:
      - id: extension_type
        type: u1
      - id: block
        type: subblock

  subblock:
    seq:
      - id: block_size
        type: u1
      - id: block_data
        size: block_size

  image_data_block:
    seq:
      - id: subblocks
        type: subblock
        repeat: eos