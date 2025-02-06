meta:
  id: gif
  endian: le

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

types:
  header:
    seq:
      - id: signature
        content: 'GIF'
      - id: version
        size: 3

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
    instances:
      global_color_table_flag:
        value: packed_fields & 0x80
      color_resolution:
        value: (packed_fields & 0x70) >> 4
      sort_flag:
        value: (packed_fields & 0x08) >> 3
      size_of_global_color_table:
        value: packed_fields & 0x07

  global_color_table:
    seq:
      - id: entries
        type: color_table_entry
        repeat: expr
        repeat-expr: 3 * (1 << logical_screen_descriptor.size_of_global_color_table + 1)

  color_table_entry:
    seq:
      - id: red
        size: 1
      - id: green
        size: 1
      - id: blue
        size: 1

  image:
    seq:
      - id: image_separator
        size: 1
        content: ','
      - id: image_descriptor
        type: image_descriptor
      - id: local_color_table
        type: global_color_table
        if: image_descriptor.local_color_table_flag == 1
      - id: image_data
        type: image_data
      - id: extensions
        type: extension
        repeat: eos

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
      - id: packed_fields
        type: u1
    instances:
      local_color_table_flag:
        value: packed_fields & 0x80
      interlace_flag:
        value: (packed_fields & 0x40) >> 6
      sort_flag:
        value: (packed_fields & 0x20) >> 5
      reserved:
        value: (packed_fields & 0x18) >> 3
      size_of_local_color_table:
        value: packed_fields & 0x07

  image_data:
    seq:
      - id: lzw_min_code_size
        size: 1
      - id: data
        type: lzw_compressed_data

  lzw_compressed_data:
    seq:
      - id: sub_blocks
        type: sub_block
        repeat: eos

  sub_block:
    seq:
      - id: length
        size: 1
      - id: data
        size: 1
        repeat: expr
        repeat-expr: length

  extension:
    seq:
      - id: extension_introducer
        size: 1
        content: '!'
      - id: extension_label
        size: 1
      - id: extension_data
        type:
          switch-on: extension_label
          cases:
            'F': graphic_control_extension
            '+': plain_text_extension
            'A': application_extension

  graphic_control_extension:
    seq:
      - id: block_size
        size: 1
      - id: packed_fields
        size: 1
      - id: delay_time
        type: u2
      - id: transparent_color_index
        size: 1
      - id: block_terminator
        size: 1
        terminator: 0

    instances:
      disposal_method:
        value: (packed_fields & 0x1c) >> 2
      user_input_flag:
        value: (packed_fields & 0x02) >> 1
      transparent_color_flag:
        value: packed_fields & 0x01

  plain_text_extension:
    seq:
      - id: block_size
        size: 1
      - id: text_grid_left
        type: u2
      - id: text_grid_top
        type: u2
      - id: text_grid_width
        type: u2
      - id: text_grid_height
        type: u2
      - id: cell_width
        size: 1
      - id: cell_height
        size: 1
      - id: text_foreground_color_index
        size: 1
      - id: text_background_color_index
        size: 1
      - id: block_terminator
        size: 1
        terminator: 0

  application_extension:
    seq:
      - id: block_size
        size: 1
      - id: application_identifier
        size: expr
        size-eos: false
        repeat-expr: 8
      - id: application_data
        size: eos