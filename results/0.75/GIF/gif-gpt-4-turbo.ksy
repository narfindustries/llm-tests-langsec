meta:
  id: gif
  file-extension: gif
  endian: le
  license: CC0-1.0
  title: Graphics Interchange Format (GIF)
doc: |
  The Graphics Interchange Format (GIF) is a bitmap image format developed
  by CompuServe, using the LZW lossless data compression, can be used to display
  animation, allows a separate palette of up to 256 colors for each frame.
seq:
  - id: header
    type: header
  - id: logical_screen
    type: logical_screen_descriptor
  - id: global_color_table
    type: color_table
    if: header.flags.has_global_color_table == 1
    size: logical_screen.global_color_table_size
  - id: blocks
    type: block
    repeat: eos

types:
  header:
    seq:
      - id: magic
        contents: "GIF"
      - id: version
        contents: ["89a", "87a"]
      - id: flags
        type: global_flags

  global_flags:
    seq:
      - id: has_global_color_table
        type: b1
      - id: color_resolution
        type: b3
      - id: sort_flag
        type: b1
      - id: size_of_global_color_table
        type: b3
    instances:
      has_global_color_table_i:
        value: has_global_color_table == 1

  logical_screen_descriptor:
    seq:
      - id: screen_width
        type: u2
      - id: screen_height
        type: u2
      - id: flags
        type: screen_flags
    instances:
      global_color_table_size:
        value: '(2 ** (flags.size_of_global_color_table + 1)) * 3'

  screen_flags:
    seq:
      - id: has_global_color_table
        type: b1
      - id: color_resolution
        type: b3
      - id: sort_flag
        type: b1
      - id: size_of_global_color_table
        type: b3

  color_table:
    seq:
      - id: colors
        type: rgb
        repeat: expr
        repeat-expr: '_parent.logical_screen.global_color_table_size // 3'

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
        pos: 1
        type:
          switch-on: block_type
          cases:
            0x2c: image_block
            0x21: extension_block

  image_block:
    seq:
      - id: image_descriptor
        type: image_descriptor
      - id: local_color_table
        type: color_table
        if: image_descriptor.flags.has_local_color_table == 1
        size: image_descriptor.local_color_table_size
      - id: image_data
        type: image_data

  extension_block:
    seq:
      - id: extension_type
        type: u1
      - id: block_size
        type: u1
      - id: block_body
        size: block_size

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
        type: image_descriptor_flags
    instances:
      local_color_table_size:
        value: '(2 ** (flags.size_of_local_color_table + 1)) * 3'

  image_descriptor_flags:
    seq:
      - id: has_local_color_table
        type: b1
      - id: interlace
        type: b1
      - id: sort
        type: b1
      - id: reserved
        type: b2
      - id: size_of_local_color_table
        type: b3

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