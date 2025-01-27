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
    if: logical_screen_descriptor.global_color_table_flag
    size: logical_screen_descriptor.global_color_table_size * 3
  - id: blocks
    type: block
    repeat: until
    repeat-until: _.block_type == block_type::trailer
types:
  header:
    seq:
      - id: signature
        contents: [0x47, 0x49, 0x46]
      - id: version
        type: str
        size: 3
        encoding: ascii
  logical_screen_descriptor:
    seq:
      - id: canvas_width
        type: u2
      - id: canvas_height
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
      global_color_table_size:
        value: 2 << (packed_fields & 0b00000111)
  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: _parent.global_color_table_size
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
      - id: block_data
        type:
          switch-on: block_type
          cases:
            'block_type::image_descriptor': image_descriptor
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
        type: u1
      - id: local_color_table
        type: color_table
        if: local_color_table_flag
        size: local_color_table_size * 3
      - id: image_data
        type: image_data
    instances:
      local_color_table_flag:
        value: (packed_fields & 0b10000000) != 0
      interlace_flag:
        value: (packed_fields & 0b01000000) != 0
      local_color_table_size:
        value: 2 << (packed_fields & 0b00000111)
  image_data:
    seq:
      - id: lzw_min_code_size
        type: u1
      - id: subblocks
        type: subblock
        repeat: until
        repeat-until: _.length == 0
  extension:
    seq:
      - id: extension_type
        type: u1
        enum: extension_type
      - id: extension_data
        type:
          switch-on: extension_type
          cases:
            'extension_type::graphic_control': graphic_control_extension
            'extension_type::comment': comment_extension
            'extension_type::application': application_extension
            _: unknown_extension
  graphic_control_extension:
    seq:
      - id: block_size
        contents: [4]
      - id: packed_fields
        type: u1
      - id: delay_time
        type: u2
      - id: transparent_color_index
        type: u1
      - id: block_terminator
        contents: [0]
  comment_extension:
    seq:
      - id: subblocks
        type: subblock
        repeat: until
        repeat-until: _.length == 0
  application_extension:
    seq:
      - id: block_size
        contents: [11]
      - id: application_identifier
        type: str
        size: 8
        encoding: ascii
      - id: application_authentication_code
        type: str
        size: 3
        encoding: ascii
      - id: subblocks
        type: subblock
        repeat: until
        repeat-until: _.length == 0
  unknown_extension:
    seq:
      - id: subblocks
        type: subblock
        repeat: until
        repeat-until: _.length == 0
  subblock:
    seq:
      - id: length
        type: u1
      - id: data
        type: str
        size: length
        encoding: ascii
        if: length > 0
  trailer:
    seq:
      - id: marker
        contents: [0x3b]
enums:
  block_type:
    0x2c: image_descriptor
    0x21: extension
    0x3b: trailer
  extension_type:
    0xf9: graphic_control
    0xfe: comment
    0xff: application