meta:
  id: gif
  title: GIF
  file-extension: gif
  endian: le

seq:
  - id: header
    type: header

  - id: logical_screen_descriptor
    type: logical_screen_descriptor

  - id: global_color_table
    type: color_table
    size: logical_screen_descriptor.has_global_color_table ? (3 * (2 ** (logical_screen_descriptor.global_color_table_size + 1))) : 0

  - id: blocks
    type: block
    repeat: until
    repeat-until: _.terminator == 0x3b

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
      - id: width
        type: u2

      - id: height
        type: u2

      - id: flags
        type: u1

      - id: background_color_index
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

      global_color_table_size:
        value: flags & 0x07

  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: _size / 3

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
      - id: introducer
        type: u1

      - id: data
        size: _root._io.size - _io.pos
        type:
          switch-on: introducer
          cases:
            0x21: extension
            0x2c: image_descriptor
            0x3b: terminator

  extension:
    seq:
      - id: label
        type: u1

      - id: block_size
        type: u1

      - id: data
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
        type: u1

      - id: local_color_table
        type: color_table
        size: has_local_color_table ? (3 * (2 ** (local_color_table_size + 1))) : 0

    instances:
      has_local_color_table:
        value: (flags & 0x80) != 0

      interlace_flag:
        value: (flags & 0x40) != 0

      is_sorted:
        value: (flags & 0x20) != 0

      reserved:
        value: (flags & 0x18) >> 3

      local_color_table_size:
        value: flags & 0x07

  terminator:
    seq:
      - id: block_terminator
        contents: "\x00"