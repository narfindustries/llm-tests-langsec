meta:
  id: gif
  title: Graphics Interchange Format
seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: global_color_table
    if: header.version == "89a" and logical_screen_descriptor.packed_fields.global_color_table_flag
  - id: images
    type: image
    repeat: eos
types:
  header:
    seq:
      - id: signature
        size: 3
        encoding: ascii
      - id: version
        size: 3
        encoding: ascii
  logical_screen_descriptor:
    seq:
      - id: width
        size: 2
      - id: height
        size: 2
      - id: packed_fields
        type: packed_fields
      - id: background_color_index
        size: 1
      - id: pixel_aspect_ratio
        size: 1
    types:
      packed_fields:
        bits: 8
        seq:
          - id: global_color_table_flag
            type: bits_type
            num_bits: 1
          - id: color_resolution
            type: bits_type
            num_bits: 3
          - id: sort_flag
            type: bits_type
            num_bits: 1
          - id: size_of_global_color_table
            type: bits_type
            num_bits: 3
  global_color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: (2 ** (logical_screen_descriptor.packed_fields.size_of_global_color_table + 1))
    types:
      rgb:
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
        contents: [44]
      - id: left
        size: 2
      - id: top
        size: 2
      - id: width
        size: 2
      - id: height
        size: 2
      - id: packed_fields
        type: packed_fields
      - id: local_color_table
        type: local_color_table
        if: packed_fields.local_color_table_flag
      - id: image_data
        type: image_data
    types:
      packed_fields:
        bits: 8
        seq:
          - id: local_color_table_flag
            type: bits_type
            num_bits: 1
          - id: interlace_flag
            type: bits_type
            num_bits: 1
          - id: sort_flag
            type: bits_type
            num_bits: 1
          - id: reserved
            type: bits_type
            num_bits: 2
          - id: size_of_local_color_table
            type: bits_type
            num_bits: 3
      local_color_table:
        seq:
          - id: entries
            type: rgb
            repeat: expr
            repeat-expr: (2 ** (packed_fields.size_of_local_color_table + 1))
      image_data:
        seq:
          - id: blocks
            type: image_data_block
            repeat: until block_size == 0
        types:
          image_data_block:
            seq:
              - id: block_size
                size: 1
              - id: data
                size: block_size
  graphic_control_extension:
    seq:
      - id: extension_introducer
        contents: [33]
      - id: extension_label
        contents: [70]
      - id: block_size
        size: 1
      - id: packed_fields
        type: packed_fields
      - id: delay_time
        size: 2
      - id: transparent_color_index
        size: 1
      - id: block_terminator
        contents: [0]
    types:
      packed_fields:
        bits: 8
        seq:
          - id: reserved
            type: bits_type
            num_bits: 3
          - id: disposal_method
            type: bits_type
            num_bits: 3
          - id: user_input_flag
            type: bits_type
            num_bits: 1
          - id: transparent_color_flag
            type: bits_type
            num_bits: 1
  comment_extension:
    seq:
      - id: extension_introducer
        contents: [33]
      - id: extension_label
        contents: [59]
      - id: blocks
        type: comment_data_block
        repeat: until block_size == 0
    types:
      comment_data_block:
        seq:
          - id: block_size
            size: 1
          - id: data
            size: block_size
  application_extension:
    seq:
      - id: extension_introducer
        contents: [33]
      - id: extension_label
        contents: [43]
      - id: blocks
        type: application_data_block
        repeat: until block_size == 0
    types:
      application_data_block:
        seq:
          - id: block_size
            size: 1
          - id: data
            size: block_size