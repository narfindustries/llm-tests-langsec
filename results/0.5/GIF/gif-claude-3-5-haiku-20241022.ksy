meta:
  id: gif
  file-extension: gif
  endian: le
  encoding: ascii

seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: color_table
    if: logical_screen_descriptor.global_color_table_flag
  - id: blocks
    type: block
    repeat: eos

types:
  header:
    seq:
      - id: signature
        contents: [0x47, 0x49, 0x46]
      - id: version
        type: str
        size: 3
        encoding: ascii
        valid:
          any-of: 
            - '"87a"'
            - '"89a"'

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
        value: (packed_fields & 0b10000000) != 0
      color_resolution:
        value: (packed_fields & 0b01110000) >> 4
      sort_flag:
        value: (packed_fields & 0b00001000) != 0
      global_color_table_size:
        value: packed_fields & 0b00000111

  color_table:
    params:
      - id: num_colors
        type: u1
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: num_colors
    types:
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
    types:
      extension_block:
        seq:
          - id: extension_type
            type: u1
          - id: extension_data
            type:
              switch-on: extension_type
              cases:
                0xF9: graphic_control_extension
                0xFE: comment_extension
                0xFF: application_extension
      graphic_control_extension:
        seq:
          - id: block_size
            contents: [0x04]
          - id: packed_fields
            type: u1
          - id: delay_time
            type: u2
          - id: transparent_color_index
            type: u1
        instances:
          disposal_method:
            value: (packed_fields & 0b00011100) >> 2
          user_input_flag:
            value: (packed_fields & 0b00000010) != 0
          transparent_color_flag:
            value: (packed_fields & 0b00000001) != 0
      comment_extension:
        seq:
          - id: comment_data
            type: str
            size-eos: true
            encoding: ascii
      application_extension:
        seq:
          - id: application_identifier
            type: str
            size: 8
            encoding: ascii
          - id: authentication_code
            type: str
            size: 3
            encoding: ascii
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
          - id: local_color_table
            type: color_table
            if: local_color_table_flag
            params:
              - id: num_colors
                value: 2 ** (packed_fields & 0b00000111 + 1)
          - id: image_data
            type: image_data
        instances:
          local_color_table_flag:
            value: (packed_fields & 0b10000000) != 0
          interlace_flag:
            value: (packed_fields & 0b01000000) != 0
          sort_flag:
            value: (packed_fields & 0b00100000) != 0
          local_color_table_size:
            value: packed_fields & 0b00000111
      image_data:
        seq:
          - id: lzw_minimum_code_size
            type: u1
          - id: data_blocks
            type: data_block
            repeat: until
            repeat-until: _.block_size == 0
      data_block:
        seq:
          - id: block_size
            type: u1
          - id: data
            type: u1
            repeat: expr
            repeat-expr: block_size
            if: block_size > 0
    instances:
      block_content:
        type:
          switch-on: block_type
          cases:
            0x21: extension_block
            0x2C: image_descriptor
            0x3B: none

enums:
  block_type:
    0x21: extension
    0x2C: image_descriptor
    0x3B: trailer