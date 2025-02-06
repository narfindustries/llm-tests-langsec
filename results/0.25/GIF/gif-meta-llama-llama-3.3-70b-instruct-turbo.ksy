seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: global_color_table
    if: logical_screen_descriptor.global_color_table_flag == 1
  - id: images
    type: image
    repeat: eos
  - id: trailer
    type: trailer

types:
  header:
    seq:
      - id: signature
        size: 3
      - id: version
        size: 3
    if: signature == b'GIF'

  logical_screen_descriptor:
    seq:
      - id: width
        type: u2
      - id: height
        type: u2
      - id: packed_fields
        type: u1
      - id: background_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
    attrs:
      global_color_table_flag:
        value: packed_fields & 0x80
      color_resolution:
        value: (packed_fields & 0x70) >> 4
      sort_flag:
        value: (packed_fields & 0x0c) >> 2
      size_of_global_color_table:
        value: packed_fields & 0x07

  global_color_table:
    seq:
      - id: entries
        type: color_table_entry
        repeat: expr
        repeat-expr: (1 << (logical_screen_descriptor.size_of_global_color_table + 1))

  color_table_entry:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

  image:
    seq:
      - id: image_separator
        size: 1
      - id: left_position
        type: u2
      - id: top_position
        type: u2
      - id: width
        type: u2
      - id: height
        type: u2
      - id: packed_fields
        type: u1
      - id: local_color_table
        type: local_color_table
        if: packed_fields & 0x80 == 0x80
      - id: image_data
        type: image_data
    attrs:
      interlace_flag:
        value: packed_fields & 0x40
      sort_flag:
        value: packed_fields & 0x20

  local_color_table:
    seq:
      - id: entries
        type: color_table_entry
        repeat: expr
        repeat-expr: (1 << (packed_fields & 0x07 + 1))

  image_data:
    seq:
      - id: lzw_minimum_code_size
        type: u1
      - id: data
        type: lzw_compressed_data

  lzw_compressed_data:
    seq:
      - id: sub_blocks
        type: sub_block
        repeat: until

  sub_block:
    seq:
      - id: length
        type: u1
      - id: data
        type: u1
        repeat: expr
        repeat-expr: length

  trailer:
    size: 1
    value: 0x3b

  extension:
    seq:
      - id: introducer
        size: 1
      - id: data
        type: extension_data
        if: introducer == 0xf9
      - id: data
        type: comment_extension
        if: introducer == 0xfe
      - id: data
        type: application_extension
        if: introducer == 0xff

  extension_data:
    seq:
      - id: label
        size: 1
      - id: block_size
        type: u1
      - id: data
        type: u1
        repeat: expr
        repeat-expr: block_size

  comment_extension:
    seq:
      - id: label
        size: 1
      - id: block_size
        type: u1
      - id: data
        type: u1
        repeat: expr
        repeat-expr: block_size

  application_extension:
    seq:
      - id: label
        size: 1
      - id: block_size
        type: u1
      - id: application_identifier
        size: 8
      - id: application_authentication_code
        size: 3
      - id: data
        type: u1
        repeat: expr
        repeat-expr: block_size - 11