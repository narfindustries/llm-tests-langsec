meta:
  id: gif
  file-extension: gif
  endian: le
  title: Graphics Interchange Format (GIF)
doc: |
  GIF (Graphics Interchange Format) is an image format developed by CompuServe in 1987.
  This format supports both animated and static images. It uses LZW compression to reduce
  the file size without degrading the visual quality.
seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: rgb
    repeat: expr
    repeat-expr: logical_screen_descriptor.global_color_table_flag ? (2 ** (logical_screen_descriptor.size_of_global_color_table + 1)) : 0
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
        contents: '89a'

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
      global_color_table_flag:
        value: (packed_fields & 0x80) != 0
      color_resolution:
        value: ((packed_fields & 0x70) >> 4) + 1
      sort_flag:
        value: (packed_fields & 0x08) != 0
      size_of_global_color_table:
        value: packed_fields & 0x07

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
      - id: left_pos
        type: u2
      - id: top_pos
        type: u2
      - id: width
        type: u2
      - id: height
        type: u2
      - id: packed_fields
        type: u1
      - id: lzw_min_code_size
        type: u1
      - id: image_data
        type: image_data_block
    instances:
      local_color_table_flag:
        value: (packed_fields & 0x80) != 0
      interlace_flag:
        value: (packed_fields & 0x40) != 0
      sort_flag:
        value: (packed_fields & 0x20) != 0
      size_of_local_color_table:
        value: packed_fields & 0x07
      local_color_table:
        pos: _io.pos
        io: _io
        type: rgb
        repeat: expr
        repeat-expr: local_color_table_flag ? (2 ** (size_of_local_color_table + 1)) : 0

  image_data_block:
    seq:
      - id: data
        type: u1
        repeat: eos

  extension:
    seq:
      - id: label
        type: u1
      - id: block_size
        type: u1
      - id: data
        size: block_size

  terminator:
    seq: []