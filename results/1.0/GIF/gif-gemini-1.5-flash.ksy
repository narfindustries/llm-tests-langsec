meta:
  id: gif-gemini-1
  title: GIF (Gemini 1.5 Flash)
  license: Apache-2.0
  compiler: kaitaistruct
  endian: be

seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: global_color_table
    if: header.global_color_table_flag
  - id: blocks
    type: blocks

types:
  header:
    seq:
      - id: signature
        type: u4
        doc: "Signature: 'GIF89a'"
      - id: version
        type: strz
        size: 3

  logical_screen_descriptor:
    seq:
      - id: width
        type: u2
      - id: height
        type: u2
      - id: packed_fields
        type: u1
        instances:
          - id: global_color_table_flag
            type: bool
            bits: 1
          - id: color_resolution
            type: u3
            bits: 3
          - id: sort_flag
            type: bool
            bits: 1
          - id: size_of_global_color_table
            type: u3
            bits: 3
      - id: background_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1

  global_color_table:
    seq:
      - id: entries
        type: color_table_entry
        repeat: (1 << (logical_screen_descriptor.packed_fields.size_of_global_color_table + 1))

  color_table_entry:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1


  blocks:
    seq:
      - id: block
        type: block
        repeat: eos

  block:
    switch: type
    cases:
      image_descriptor:
        type: image_descriptor
      graphic_control_extension:
        type: graphic_control_extension
      comment_extension:
        type: comment_extension
      application_extension:
        type: application_extension
      plain_text_extension:
        type: plain_text_extension
      terminator:
        type: terminator
      default:
          type: unknown_block

  image_descriptor:
    seq:
      - id: image_separator
        type: u1
      - id: image_left_position
        type: u2
      - id: image_top_position
        type: u2
      - id: image_width
        type: u2
      - id: image_height
        type: u2
      - id: packed_fields
        type: u1
        instances:
          - id: local_color_table_flag
            type: bool
            bits: 1
          - id: interlace_flag
            type: bool
            bits: 1
          - id: sort_flag
            type: bool
            bits: 1
          - id: reserved
            type: u5
            bits: 5
      - id: local_color_table
        type: local_color_table
        if: packed_fields.local_color_table_flag
      - id: image_data
        type: image_data


  local_color_table:
    seq:
      - id: entries
        type: color_table_entry
        repeat: (1 << ( (packed_fields.size_of_local_color_table + 1) ))


  image_data:
    seq:
      - id: lzw_minimum_code_size
        type: u1
      - id: data_subblocks
        type: data_subblocks


  data_subblocks:
      seq:
          - id: subblock
              type: subblock
              repeat: eos

  subblock:
      seq:
          - id: size
              type: u1
          - id: data
              type: bytes
              size: size

  graphic_control_extension:
    seq:
      - id: extension_introducer
        type: u1
      - id: label
        type: u1
      - id: packed_fields
        type: u1
        instances:
          - id: disposal_method
            type: u3
            bits: 3
          - id: user_input_flag
            type: bool
            bits: 1
          - id: transparency_flag
            type: bool
            bits: 1
      - id: delay_time
        type: u2
      - id: transparent_color_index
        type: u1
      - id: block_terminator
        type: u1

  comment_extension:
    seq:
      - id: extension_introducer
        type: u1
      - id: label
        type: u1
      - id: comment
        type: subblocks

  application_extension:
    seq:
      - id: extension_introducer
        type: u1
      - id: label
        type: u1
      - id: application_identifier
        type: strz
        size: 8
      - id: authentication_code
        type: strz
        size: 3
      - id: data_subblocks
        type: data_subblocks

  plain_text_extension:
    seq:
      - id: extension_introducer
        type: u1
      - id: label
        type: u1
      - id: text_grid_left_position
        type: u2
      - id: text_grid_top_position
        type: u2
      - id: text_grid_width
        type: u1
      - id: text_grid_height
        type: u1
      - id: text_grid_cell_width
        type: u1
      - id: text_grid_cell_height
        type: u1
      - id: text_data
        type: subblocks

  terminator:
    seq:
      - id: terminator
        type: u1

  unknown_block:
    seq:
      - id: block_type
        type: u1
      - id: block_data
        type: bytes


