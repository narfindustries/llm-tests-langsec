meta:
  id: gif
  file-extension: gif
  endian: le
  license: CC0-1.0
  title: Graphics Interchange Format (GIF)
doc: |
  The Graphics Interchange Format is a bitmap image format that was developed
  by a team at the online services provider CompuServe led by American computer scientist Steve Wilhite.
  It was first released in 1987 and has since come into widespread usage on the World Wide Web due to its wide support and portability.

seq:
  - id: header
    type: header

  - id: logical_screen
    type: logical_screen_descriptor

  - id: global_color_table
    type: color_table
    if: logical_screen.has_color_table

  - id: blocks
    type: block
    repeat: eos

types:
  header:
    seq:
      - id: magic
        contents: 'GIF'
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
      has_color_table:
        value: flags & 0x80 != 0
      color_resolution:
        value: ((flags & 0x70) >> 4) + 1
      sort_flag:
        value: (flags & 0x08) != 0
      size_of_global_color_table:
        value: (flags & 0x07) + 1

  color_table:
    params:
      num_colors: expr
    seq:
      - id: colors
        type: rgb
        repeat: expr
        repeat-expr: num_colors
    instances:
      num_colors_calc:
        value: 2 ** num_colors

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
    instances:
      body:
        pos: _io.pos
        io: _io
        type:
          switch-on: block_type
          cases:
            0x2c: image_block
            0x21: extension
            0x3b: terminator

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
        if: flags & 0x80 != 0
      - id: lzw_min_code_size
        type: u1
      - id: data_blocks
        type: data_block
        repeat: eos

  data_block:
    seq:
      - id: block_size
        type: u1
      - id: block_data
        size: block_size

  extension:
    seq:
      - id: label
        type: u1
      - id: block_size
        type: u1
      - id: block_data
        size: block_size

  terminator:
    seq: []
