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
    size: >-
      ((1 << (logical_screen_descriptor.packed_fields.global_color_table_size + 1)) * 3)
  - id: blocks
    type: block
    repeat: until
    repeat-until: _.block_type == block_types::trailer

types:
  header:
    seq:
      - id: signature
        contents: "GIF"
      - id: version
        type: str
        size: 3
        encoding: ascii
        valid:
          any-of: 
            - "87a"
            - "89a"

  logical_screen_descriptor:
    seq:
      - id: canvas_width
        type: u2
      - id: canvas_height
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
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: 1 << (_parent.logical_screen_descriptor.packed_fields.global_color_table_size + 1)

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
        enum: block_types
    types:
      graphic_control_extension:
        seq:
          - id: extension_introducer
            contents: [0x21]
          - id: graphic_control_label
            contents: [0xf9]
          - id: block_size
            contents: [0x04]
          - id: packed_fields
            type: packed_graphic_control
          - id: delay_time
            type: u2
          - id: transparent_color_index
            type: u1
          - id: block_terminator
            contents: [0x00]

      image_descriptor:
        seq:
          - id: separator
            contents: [0x2c]
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
            type: color_table
            if: packed_fields.local_color_table_flag
            size: >-
              ((1 << (packed_fields.local_color_table_size + 1)) * 3)
          - id: lzw_minimum_code_size
            type: u1
          - id: image_data
            type: image_data_block

      comment_extension:
        seq:
          - id: extension_introducer
            contents: [0x21]
          - id: comment_label
            contents: [0xfe]
          - id: comment_data
            type: comment_block

      application_extension:
        seq:
          - id: extension_introducer
            contents: [0x21]
          - id: application_label
            contents: [0xff]
          - id: block_size
            contents: [0x0b]
          - id: application_identifier
            type: str
            size: 8
            encoding: ascii
          - id: application_authentication_code
            type: str
            size: 3
            encoding: ascii
          - id: application_data
            type: application_data_block

    instances:
      parsed_block:
        value: >-
          block_type == block_types::extension_introducer ? (
            _io.peek_u1 == 0xf9 ? graphic_control_extension :
            _io.peek_u1 == 0xfe ? comment_extension :
            _io.peek_u1 == 0xff ? application_extension :
            null
          ) : (
            block_type == block_types::image_separator ? image_descriptor : null
          )

  packed_graphic_control:
    seq:
      - id: reserved
        type: b3
      - id: disposal_method
        type: b3
      - id: user_input_flag
        type: b1
      - id: transparent_color_flag
        type: b1

  packed_image_descriptor:
    seq:
      - id: local_color_table_flag
        type: b1
      - id: interlace_flag
        type: b1
      - id: sort_flag
        type: b1
      - id: reserved
        type: b2
      - id: local_color_table_size
        type: b3

  image_data_block:
    seq:
      - id: blocks
        type: data_sub_block
        repeat: until
        repeat-until: _.length == 0

  data_sub_block:
    seq:
      - id: length
        type: u1
      - id: data
        type: u1
        repeat: expr
        repeat-expr: length

  comment_block:
    seq:
      - id: blocks
        type: data_sub_block
        repeat: until
        repeat-until: _.length == 0

  application_data_block:
    seq:
      - id: blocks
        type: data_sub_block
        repeat: until
        repeat-until: _.length == 0

enums:
  block_types:
    0x21: extension_introducer
    0x2c: image_separator
    0x3b: trailer