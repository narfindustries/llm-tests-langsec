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
    if: logical_screen_descriptor.has_global_color_table
  - id: blocks
    type: block
    repeat: until
    repeat-until: _.type == 'trailer'
types:
  header:
    seq:
      - id: signature
        contents: 'GIF'
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
        value: (flags >> 4) & 0x07
      is_sorted:
        value: (flags & 0x08) != 0
      global_color_table_size:
        value: 1 << ((flags & 0x07) + 1)
  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: _parent.global_color_table_size
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
      - id: type
        type: u1
      - id: data
        type:
          switch-on: type
          cases:
            0x2C: image_descriptor
            0x21: extension
            0x3B: trailer
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
        if: has_local_color_table
      - id: lzw_minimum_code_size
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
      local_color_table_size:
        value: 1 << ((flags & 0x07) + 1)
  extension:
    seq:
      - id: label
        type: u1
      - id: body
        type:
          switch-on: label
          cases:
            0xF9: graphic_control_extension
            0xFE: comment_extension
            0x01: plain_text_extension
            0xFF: application_extension
  graphic_control_extension:
    seq:
      - id: block_size
        contents: [0x04]
      - id: flags
        type: u1
      - id: delay_time
        type: u2
      - id: transparent_color_index
        type: u1
      - id: terminator
        contents: [0x00]
    instances:
      reserved:
        value: (flags >> 5) & 0x07
      disposal_method:
        value: (flags >> 2) & 0x07
      user_input_flag:
        value: (flags & 0x02) != 0
      transparent_color_flag:
        value: (flags & 0x01) != 0
  comment_extension:
    seq:
      - id: comments
        type: subblocks
  plain_text_extension:
    seq:
      - id: block_size
        contents: [0x0C]
      - id: text_grid_left
        type: u2
      - id: text_grid_top
        type: u2
      - id: text_grid_width
        type: u2
      - id: text_grid_height
        type: u2
      - id: character_cell_width
        type: u1
      - id: character_cell_height
        type: u1
      - id: text_foreground_color_index
        type: u1
      - id: text_background_color_index
        type: u1
      - id: plain_text_data
        type: subblocks
  application_extension:
    seq:
      - id: block_size
        contents: [0x0B]
      - id: application_identifier
        size: 8
        type: str
        encoding: ASCII
      - id: application_auth_code
        size: 3
        type: str
        encoding: ASCII
      - id: application_data
        type: subblocks
  subblocks:
    seq:
      - id: blocks
        type: subblock
        repeat: until
        repeat-until: _.size == 0
  subblock:
    seq:
      - id: size
        type: u1
      - id: bytes
        size: size
  trailer:
    seq:
      - id: terminator
        contents: [0x3B]