meta:
  id: gif
  file-extension: gif
  endian: le

seq:
  - id: header
    type: header
  - id: logical_screen
    type: logical_screen
  - id: blocks
    type: block
    repeat: until
    repeat-until: _.block_type == block_type::end_of_file

types:
  header:
    seq:
      - id: magic
        contents: 'GIF'
      - id: version
        type: str
        size: 3
        encoding: ASCII

  logical_screen:
    seq:
      - id: screen_width
        type: u2
      - id: screen_height
        type: u2
      - id: flags
        type: packed_field
      - id: bg_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
      - id: global_color_table
        type: color_table
        if: flags.has_global_color_table
        size: flags.global_color_table_size * 3

  packed_field:
    seq:
      - id: flags
        type: u1
    instances:
      has_global_color_table:
        value: (flags & 0b10000000) != 0
      color_resolution:
        value: ((flags & 0b01110000) >> 4) + 1
      global_color_table_sorted:
        value: (flags & 0b00001000) != 0
      global_color_table_size:
        value: 1 << ((flags & 0b00000111) + 1)

  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: eos

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
      - id: body
        type:
          switch-on: block_type
          cases:
            'block_type::extension': extension_block
            'block_type::image': image_block

  extension_block:
    seq:
      - id: extension_type
        type: u1
        enum: extension_type
      - id: body
        type:
          switch-on: extension_type
          cases:
            'extension_type::graphics_control': graphics_control_ext
            'extension_type::comment': comment_ext
            'extension_type::plain_text': plain_text_ext
            'extension_type::application': application_ext

  graphics_control_ext:
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

  comment_ext:
    seq:
      - id: subblocks
        type: subblocks

  plain_text_ext:
    seq:
      - id: block_size
        contents: [0x0C]
      - id: grid_left
        type: u2
      - id: grid_top
        type: u2
      - id: grid_width
        type: u2
      - id: grid_height
        type: u2
      - id: cell_width
        type: u1
      - id: cell_height
        type: u1
      - id: fg_color_index
        type: u1
      - id: bg_color_index
        type: u1
      - id: subblocks
        type: subblocks

  application_ext:
    seq:
      - id: block_size
        contents: [0x0B]
      - id: application_identifier
        type: str
        size: 8
        encoding: ASCII
      - id: application_auth_code
        size: 3
      - id: subblocks
        type: subblocks

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
        type: image_flags
      - id: local_color_table
        type: color_table
        if: flags.has_local_color_table
        size: flags.local_color_table_size * 3
      - id: image_data
        type: image_data

  image_flags:
    seq:
      - id: flags
        type: u1
    instances:
      has_local_color_table:
        value: (flags & 0b10000000) != 0
      interlace:
        value: (flags & 0b01000000) != 0
      sort:
        value: (flags & 0b00100000) != 0
      local_color_table_size:
        value: 1 << ((flags & 0b00000111) + 1)

  image_data:
    seq:
      - id: lzw_min_code_size
        type: u1
      - id: subblocks
        type: subblocks

  subblocks:
    seq:
      - id: entries
        type: subblock
        repeat: until
        repeat-until: _.block_size == 0

  subblock:
    seq:
      - id: block_size
        type: u1
      - id: data
        size: block_size

enums:
  block_type:
    0x21: extension
    0x2c: image
    0x3b: end_of_file

  extension_type:
    0xf9: graphics_control
    0xfe: comment
    0x01: plain_text
    0xff: application