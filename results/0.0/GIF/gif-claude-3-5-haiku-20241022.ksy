meta:
  id: gif
  file-extension: gif
  endian: le

seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: color_table
    if: logical_screen_descriptor.packed_fields.global_color_table_flag
  - id: blocks
    type: block
    repeat: until
    repeat-until: _io.is_eof or _.block_type == block_type::trailer

types:
  header:
    seq:
      - id: signature
        contents: "GIF"
      - id: version
        type: str
        size: 3
        valid:
          any-of: ["87a", "89a"]

  logical_screen_descriptor:
    seq:
      - id: width
        type: u2
      - id: height
        type: u2
      - id: packed_fields
        type: packed_screen_descriptor
      - id: background_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1

  packed_screen_descriptor:
    seq:
      - id: global_color_table_flag
        type: b1
      - id: color_resolution
        type: b3
      - id: sort_flag
        type: b1
      - id: global_color_table_size
        type: b3

  color_table:
    params:
      - id: num_entries
        type: u1
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: num_entries

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
      - id: contents
        type:
          switch-on: block_type
          cases:
            'block_type::image_separator': image_descriptor
            'block_type::extension': extension
            'block_type::trailer': trailer

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
        type: packed_image_descriptor
      - id: local_color_table
        type: color_table(1 << (packed_fields.local_color_table_size + 1))
        if: packed_fields.local_color_table_flag
      - id: lzw_min_code_size
        type: u1
      - id: image_data
        type: image_data_block

  packed_image_descriptor:
    seq:
      - id: local_color_table_flag
        type: b1
      - id: interlace_flag
        type: b1
      - id: sort_flag
        type: b1
      - id: local_color_table_size
        type: b3

  image_data_block:
    seq:
      - id: blocks
        type: data_block
        repeat: until
        repeat-until: _.block_size == 0

  data_block:
    seq:
      - id: block_size
        type: u1
      - id: data
        size: block_size
        if: block_size > 0

  extension:
    seq:
      - id: extension_type
        type: u1
      - id: contents
        type:
          switch-on: extension_type
          cases:
            0xf9: graphic_control_extension
            0xff: application_extension
            0xfe: comment_extension
            0x01: plain_text_extension

  graphic_control_extension:
    seq:
      - id: block_size
        contents: [4]
      - id: packed_fields
        type: packed_graphic_control
      - id: delay_time
        type: u2
      - id: transparent_color_index
        type: u1

  packed_graphic_control:
    seq:
      - id: disposal_method
        type: b3
      - id: user_input_flag
        type: b1
      - id: transparent_color_flag
        type: b1

  application_extension:
    seq:
      - id: block_size
        type: u1
      - id: identifier
        type: str
        size: 8
      - id: auth_code
        type: str
        size: 3
      - id: data_blocks
        type: data_block
        repeat: until
        repeat-until: _.block_size == 0

  comment_extension:
    seq:
      - id: data_blocks
        type: data_block
        repeat: until
        repeat-until: _.block_size == 0

  plain_text_extension:
    seq:
      - id: block_size
        type: u1
      - id: text_grid_left
        type: u2
      - id: text_grid_top
        type: u2
      - id: text_grid_width
        type: u2
      - id: text_grid_height
        type: u2
      - id: cell_width
        type: u1
      - id: cell_height
        type: u1
      - id: foreground_color_index
        type: u1
      - id: background_color_index
        type: u1
      - id: data_blocks
        type: data_block
        repeat: until
        repeat-until: _.block_size == 0

  trailer:
    seq:
      - id: terminator
        contents: [0x3b]

enums:
  block_type:
    0x2c: image_separator
    0x21: extension
    0x3b: trailer