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
    size: 'logical_screen_descriptor.global_color_table_size'
    if: 'logical_screen_descriptor.has_global_color_table'
  - id: blocks
    type: block
    repeat: until
    repeat-until: '_io.eof or _.block_type == 0x3b'

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
      - id: bg_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1

    instances:
      has_global_color_table:
        value: 'flags & 0x80 != 0'
      color_resolution:
        value: '(flags >> 4) & 0x07'
      is_sorted:
        value: 'flags & 0x08 != 0'
      global_color_table_size:
        value: '2 << (flags & 0x07)'

  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: '_root.logical_screen_descriptor.global_color_table_size'

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
            0x2c: image_block
            0x21: extension_block
            0x3b: trailer

  image_block:
    seq:
      - id: image_descriptor
        type: image_descriptor
      - id: local_color_table
        type: color_table
        size: 'image_descriptor.local_color_table_size'
        if: 'image_descriptor.has_local_color_table'
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
        value: 'flags & 0x80 != 0'
      is_interlaced:
        value: 'flags & 0x40 != 0'
      is_sorted:
        value: 'flags & 0x20 != 0'
      local_color_table_size:
        value: '2 << (flags & 0x07)'

  image_data:
    seq:
      - id: lzw_min_code_size
        type: u1
      - id: blocks
        type: subblock
        repeat: until
        repeat-until: '_.size == 0'

  subblock:
    seq:
      - id: size
        type: u1
      - id: bytes
        size: 'size'

  extension_block:
    seq:
      - id: label
        type: u1
      - id: body
        type:
          switch-on: label
          cases:
            0xf9: graphic_control_extension
            0xfe: comment_extension
            0x01: plain_text_extension
            0xff: application_extension

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
      has_transparent_color:
        value: 'flags & 0x01 != 0'
      user_input_flag:
        value: 'flags & 0x02 != 0'
      disposal_method:
        value: '(flags >> 2) & 0x07'

  comment_extension:
    seq:
      - id: comment_data
        type: subblock

  plain_text_extension:
    seq:
      - id: block_size
        contents: [0x0c]
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
      - id: fg_color_index
        type: u1
      - id: bg_color_index
        type: u1
      - id: plain_text_data
        type: subblock

  application_extension:
    seq:
      - id: block_size
        contents: [0x0b]
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

  trailer:
    seq:
      - id: terminator
        contents: [0x3b]