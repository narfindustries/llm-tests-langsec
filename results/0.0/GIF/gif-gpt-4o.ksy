meta:
  id: gif
  title: GIF
  file-extension: gif
  xref:
    mime: image/gif
  endian: le

seq:
  - id: header
    type: header

  - id: logical_screen_descriptor
    type: logical_screen_descriptor

  - id: global_color_table
    type: color_table
    if: header.flags.global_color_table_flag

  - id: blocks
    type: block
    repeat: until
    repeat-until: _.is_terminator

types:
  header:
    seq:
      - id: signature
        contents: "GIF"

      - id: version
        size: 3
        type: str
        encoding: ASCII

  logical_screen_descriptor:
    seq:
      - id: logical_screen_width
        type: u2

      - id: logical_screen_height
        type: u2

      - id: flags
        type: u1
        doc: |
          7 6 5 4 3 2 1 0
          G C R S F I X X X
          G - Global Color Table Flag
          C - Color Resolution
          R - Sort Flag
          S - Size of Global Color Table
          F - Future Use
          I - Future Use
          X - Reserved

      - id: background_color_index
        type: u1

      - id: pixel_aspect_ratio
        type: u1

  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: 2 ** (logical_screen_descriptor.flags & 0x07 + 1)

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

      - id: block_body
        type:
          switch-on: block_type
          cases:
            0x21: extension
            0x2c: image_descriptor
            0x3b: terminator

  extension:
    seq:
      - id: label
        type: u1

      - id: body
        type: subblock
        if: label != 0xf9

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
        doc: |
          7 6 5 4 3 2 1 0
          L I X X X X X X
          L - Local Color Table Flag
          I - Interlace Flag
          X - Reserved

      - id: local_color_table
        type: color_table
        if: flags & 0x80 != 0

      - id: lzw_min_code_size
        type: u1

      - id: image_data
        type: subblock

  subblock:
    seq:
      - id: size
        type: u1

      - id: bytes
        size: size
        repeat: expr
        repeat-expr: size != 0

  terminator:
    seq:
      - id: terminator
        contents: "\x3b"

    instances:
      is_terminator:
        value: true