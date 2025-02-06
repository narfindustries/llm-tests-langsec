meta:
  id: gif
  file-extension: gif
  endian: le
  title: Graphics Interchange Format (GIF)
doc: |
  GIF (Graphics Interchange Format) is an image format developed by CompuServe in 1987. This format supports both animated and static images, and is widely used on the web.
seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: color_table
    if: logical_screen_descriptor.gct_flag == 1
  - id: blocks
    type: block
    repeat: eos
types:
  header:
    seq:
      - id: signature
        size: 3
        contents: "GIF"
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
      gct_flag:
        value: (packed_fields & 0b10000000) != 0
      color_resolution:
        value: ((packed_fields & 0b01110000) >> 4) + 1
      sort_flag:
        value: (packed_fields & 0b00001000) != 0
      gct_size:
        value: 1 << ((packed_fields & 0b00000111) + 1)
  color_table:
    seq:
      - id: colors
        type: rgb
        repeat: expr
        repeat-expr: _parent.logical_screen_descriptor.gct_size
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
    types:
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
          - id: lct
            type: color_table
            if: (packed_fields & 0b10000000) != 0
          - id: image_data
            type: image_data
      extension_block:
        seq:
          - id: extension_type
            type: u1
            enum: extension_type
          - id: block_size
            type: u1
          - id: block_data
            size: block_size
enums:
  block_type:
    0x2c: image_block
    0x21: extension_block
  extension_type:
    0xf9: graphic_control
    0xfe: comment
    0x01: plain_text
    0xff: application
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