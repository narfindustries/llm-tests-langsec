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
    type: rgb
    repeat: expr
    repeat-expr: header.has_global_color_table ? (1 << (logical_screen_descriptor.size_of_gct + 1)) : 0
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
  logical_screen_descriptor:
    seq:
      - id: logical_screen_width
        type: u2
      - id: logical_screen_height
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
        value: ((flags & 0x70) >> 4) + 1
      is_sorted:
        value: (flags & 0x08) != 0
      size_of_gct:
        value: flags & 0x07

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
        size: block_size
        type:
          switch-on: block_type
          cases:
            0x21: ext_block
            0x2c: image_block
            0x3b: terminator
      - id: block_size
        type: u1

    instances:
      is_terminator:
        value: block_type == 0x3b

  ext_block:
    seq:
      - id: label
        type: u1
      - id: data
        size: block_size - 1
        type: bytes

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
        type: rgb
        repeat: expr
        repeat-expr: has_local_color_table ? (1 << (size_of_lct + 1)) : 0
      - id: lzw_min_code_size
        type: u1
      - id: image_data
        type: subblocks

    instances:
      has_local_color_table:
        value: (flags & 0x80) != 0
      is_interlaced:
        value: (flags & 0x40) != 0
      is_sorted:
        value: (flags & 0x20) != 0
      size_of_lct:
        value: flags & 0x07

  subblocks:
    seq:
      - id: subblocks
        type: subblock
        repeat: until
        repeat-until: _.size == 0

  subblock:
    seq:
      - id: size
        type: u1
      - id: bytes
        size: size