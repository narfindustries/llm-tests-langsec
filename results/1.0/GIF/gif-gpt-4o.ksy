meta:
  id: gif
  file-extension: gif
  title: GIF
  application:
    - GIF
  endian: le
doc: |
  GIF (Graphics Interchange Format) is a raster graphics file format 
  that supports both static and animated images. GIF is widely used
  for its support of animations and transparency.
seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: color_table
    if: logical_screen_descriptor.has_global_color_table
  - id: blocks
    type: block
    repeat: until
    repeat-until: block_type == 0x3B
types:
  header:
    seq:
      - id: signature
        contents: "GIF"
      - id: version
        size: 3
        type: str
  logical_screen_descriptor:
    seq:
      - id: screen_width
        type: u2
      - id: screen_height
        type: u2
      - id: flags
        type: u1
      - id: background_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
    instances:
      has_global_color_table:
        value: (flags & 0x80) != 0
      color_resolution:
        value: (flags >> 4) & 0x07
      is_sorted:
        value: (flags & 0x08) != 0
      size_of_global_color_table:
        value: 1 << ((flags & 0x07) + 1)
  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: entries_count
    params:
      - id: entries_count
        type: u4
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
      - id: body
        type:
          switch-on: block_type
          cases:
            0x2C: image_block
            0x21: extension_block
            0x3B: trailer_block
  image_block:
    seq:
      - id: image_descriptor
        type: image_descriptor
      - id: local_color_table
        type: color_table
        if: image_descriptor.has_local_color_table
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
        value: (flags & 0x80) != 0
      is_interlaced:
        value: (flags & 0x40) != 0
      is_sorted:
        value: (flags & 0x20) != 0
      size_of_local_color_table:
        value: 1 << ((flags & 0x07) + 1)
  image_data:
    seq:
      - id: lzw_minimum_code_size
        type: u1
      - id: blocks
        type: subblocks
  subblocks:
    seq:
      - id: subblocks
        type: subblock
        repeat: until
        repeat-until: _.size == 0
  subblock:
    seq:
      - id: size
        type: u1
      - id: bytes
        size: size
  extension_block:
    seq:
      - id: label
        type: u1
      - id: extension
        type:
          switch-on: label
          cases:
            0xF9: graphic_control_extension
            0xFE: comment_extension
            0x01: plain_text_extension
            0xFF: application_extension
  graphic_control_extension:
    seq:
      - id: block_size
        type: u1
        assert: block_size == 4
      - id: flags
        type: u1
      - id: delay_time
        type: u2
      - id: transparent_color_index
        type: u1
      - id: terminator
        type: u1
        assert: terminator == 0x00
  comment_extension:
    seq:
      - id: comment_data
        type: subblocks
  plain_text_extension:
    seq:
      - id: block_size
        type: u1
        assert: block_size == 12
      - id: text_grid_left
        type: u2
      - id: text_grid_top
        type: u2
      - id: text_grid_width
        type: u2
      - id: text_grid_height
        type: u2
      - id: char_width
        type: u1
      - id: char_height
        type: u1
      - id: text_fg_color_index
        type: u1
      - id: text_bg_color_index
        type: u1
      - id: plain_text_data
        type: subblocks
  application_extension:
    seq:
      - id: block_size
        type: u1
      - id: application_identifier
        size: 8
        type: str
      - id: application_authentication_code
        size: 3
        type: str
      - id: application_data
        type: subblocks
  trailer_block:
    seq:
      - id: terminator
        type: u1
        valid: terminator == 0x3B