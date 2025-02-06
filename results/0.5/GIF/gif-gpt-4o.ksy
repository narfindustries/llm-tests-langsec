meta:
  id: gif
  title: GIF
  file-extension: gif
  xref:
    forensicswiki: GIF
    mime: image/gif
    pronom: fmt/4
    wikidata: Q166090
  license: CC0-1.0
  endian: le

seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: color_table
    if: logical_screen_descriptor.has_global_color_table
    repeat: expr
    repeat-expr: logical_screen_descriptor.size_of_global_color_table
  - id: blocks
    type: block
    repeat: until
    repeat-until: _.is_trailer

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
        value: flags & 0x80 != 0
      color_resolution:
        value: (flags >> 4) & 0x07
      is_sorted:
        value: flags & 0x08 != 0
      size_of_global_color_table:
        value: 2 << (flags & 0x07)

  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: _parent.size_of_global_color_table

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
      - id: body
        type:
          switch-on: block_type
          cases:
            0x2C: image_block
            0x21: extension
            0x3B: trailer

    instances:
      is_trailer:
        value: block_type == 0x3B

  image_block:
    seq:
      - id: image_descriptor
        type: image_descriptor
      - id: local_color_table
        type: color_table
        if: image_descriptor.has_local_color_table
        repeat: expr
        repeat-expr: image_descriptor.size_of_local_color_table
      - id: image_data
        type: image_data

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
        value: flags & 0x80 != 0
      is_interlaced:
        value: flags & 0x40 != 0
      is_sorted:
        value: flags & 0x20 != 0
      size_of_local_color_table:
        value: 2 << (flags & 0x07)

  image_data:
    seq:
      - id: lzw_minimum_code_size
        type: u1
      - id: blocks
        type: subblock
        repeat: until
        repeat-until: _.len_bytes == 0

  subblock:
    seq:
      - id: len_bytes
        type: u1
      - id: bytes
        size: len_bytes

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
            _: generic_extension

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
      disposal_method:
        value: (flags >> 2) & 0x07
      user_input_flag:
        value: flags & 0x02 != 0
      transparent_color_flag:
        value: flags & 0x01 != 0

  comment_extension:
    seq:
      - id: comments
        type: subblock
        repeat: until
        repeat-until: _.len_bytes == 0

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
      - id: char_cell_width
        type: u1
      - id: char_cell_height
        type: u1
      - id: text_fg_color_index
        type: u1
      - id: text_bg_color_index
        type: u1
      - id: plain_text_data
        type: subblock
        repeat: until
        repeat-until: _.len_bytes == 0

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
        type: subblock
        repeat: until
        repeat-until: _.len_bytes == 0

  generic_extension:
    seq:
      - id: data
        type: subblock
        repeat: until
        repeat-until: _.len_bytes == 0

  trailer:
    seq: []