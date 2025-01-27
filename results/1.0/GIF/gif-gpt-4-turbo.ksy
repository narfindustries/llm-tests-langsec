meta:
  id: gif
  file-extension: gif
  endian: le
  title: Graphics Interchange Format (GIF)
  license: CC0-1.0
doc: |
  The Graphics Interchange Format is a bitmap image format that was developed
  by a team at the online services provider CompuServe led by American computer scientist Steve Wilhite.
seq:
  - id: header
    type: header
  - id: logical_screen
    type: logical_screen_descriptor
  - id: global_color_table
    type: color_table
    if: has_global_color_table
  - id: blocks
    type: block
    repeat: eos

types:
  header:
    seq:
      - id: magic
        contents: "GIF"
      - id: version
        contents: ["87a", "89a"]

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
      size_of_global_color_table:
        value: (flags & 0x07) + 1

  color_table:
    params:
      - id: num_colors
        type: s4
    seq:
      - id: colors
        type: rgb
        repeat: expr
        repeat-expr: num_colors
    instances:
      num_colors:
        value: (1 << size_of_global_color_table)

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
    cases:
      0x2c: graphic_block
      0x21: control_block

  graphic_block:
    seq:
      - id: image_descriptor
        type: image_descriptor
      - id: local_color_table
        type: color_table
        if: image_descriptor.has_local_color_table

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

  control_block:
    seq:
      - id: extension_type
        type: u1
        enum: extension
      - id: block_size
        type: u1
      - id: block_data
        size: block_size

enums:
  extension:
    0xf9: graphic_control
    0xfe: comment
    0x01: plain_text
    0xff: application