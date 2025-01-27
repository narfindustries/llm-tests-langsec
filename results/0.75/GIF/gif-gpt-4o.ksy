meta:
  id: gif
  title: GIF
  file-extension: gif
  xref:
    mime: image/gif
  endian: le
  encoding: ASCII

seq:
  - id: header
    type: header

  - id: logical_screen_descriptor
    type: logical_screen_descriptor

  - id: global_color_table
    type: rgb_triplet
    repeat: expr
    repeat-expr: logical_screen_descriptor.has_global_color_table ? (1 << (logical_screen_descriptor.size_of_global_color_table + 1)) : 0

  - id: blocks
    type: block
    repeat: until
    repeat-until: _.block_type == block_types::terminator

types:
  header:
    seq:
      - id: signature
        contents: "GIF"

      - id: version
        contents: ["87a", "89a"]

  logical_screen_descriptor:
    seq:
      - id: logical_screen_width
        type: u2

      - id: logical_screen_height
        type: u2

      - id: flags
        type: u1
        contents:
          - id: global_color_table_flag
            mask: 0x80
            enum: bool

          - id: color_resolution
            mask: 0x70
            enum: color_resolution

          - id: sort_flag
            mask: 0x08
            enum: bool

          - id: size_of_global_color_table
            mask: 0x07

      - id: background_color_index
        type: u1

      - id: pixel_aspect_ratio
        type: u1

    instances:
      has_global_color_table:
        value: flags.global_color_table_flag

  rgb_triplet:
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
        enum: block_types

      - id: block_body
        type:
          switch-on: block_type
          cases:
            'block_types::image': image_descriptor
            'block_types::extension': extension_block
            'block_types::terminator': null

  image_descriptor:
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
        contents:
          - id: local_color_table_flag
            mask: 0x80
            enum: bool

          - id: interlace_flag
            mask: 0x40
            enum: bool

          - id: sort_flag
            mask: 0x20
            enum: bool

          - id: reserved
            mask: 0x18

          - id: size_of_local_color_table
            mask: 0x07

      - id: local_color_table
        type: rgb_triplet
        repeat: expr
        repeat-expr: flags.local_color_table_flag ? (1 << (flags.size_of_local_color_table + 1)) : 0

      - id: lzw_minimum_code_size
        type: u1

      - id: image_data_blocks
        type: subblock
        repeat: until
        repeat-until: _.size == 0

  extension_block:
    seq:
      - id: label
        type: u1

      - id: extension_data
        type: subblock
        repeat: until
        repeat-until: _.size == 0

  subblock:
    seq:
      - id: size
        type: u1

      - id: bytes
        size: size

enums:
  block_types:
    image: 0x2c
    extension: 0x21
    terminator: 0x3b

  bool:
    false: 0
    true: 1

  color_resolution:
    one_bit: 0
    two_bits: 1
    three_bits: 2
    four_bits: 3
    five_bits: 4
    six_bits: 5
    seven_bits: 6
    eight_bits: 7