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
    if: logical_screen_descriptor.global_color_table_flag == 1
  - id: blocks
    type: block
    repeat: until
    repeat-until: _.type == 0x3B
types:
  header:
    seq:
      - id: signature
        contents: "GIF"
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
        value: (packed_fields >> 7) & 1
      color_resolution:
        value: ((packed_fields >> 4) & 7) + 1
      sort_flag:
        value: (packed_fields >> 3) & 1
      global_color_table_size:
        value: 1 << ((packed_fields & 7) + 1)
  color_table:
    seq:
      - id: entries
        type: color_table_entry
        repeat: expr
        repeat-expr: _root.logical_screen_descriptor.global_color_table_size
  color_table_entry:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1
  block:
    seq:
      - id: type
        type: u1
      - id: content
        type:
          switch-on: type
          cases:
            0x21: extension
            0x2C: image_descriptor
            0x3B: trailer
  extension:
    seq:
      - id: label
        type: u1
      - id: block_size
        type: u1
        if: label != 0xFE
      - id: data
        type: block_data
        repeat: until
        repeat-until: _.size == 0
  block_data:
    seq:
      - id: size
        type: u1
      - id: data
        size: size
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
        if: (packed_fields >> 7) & 1 == 1
      - id: image_data
        type: image_data
    instances:
      local_color_table_flag:
        value: (packed_fields >> 7) & 1
      interlace_flag:
        value: (packed_fields >> 6) & 1
      sort_flag:
        value: (packed_fields >> 5) & 1
      local_color_table_size:
        value: 1 << ((packed_fields & 7) + 1)
  image_data:
    seq:
      - id: lzw_min_code_size
        type: u1
      - id: blocks
        type: block_data
        repeat: until
        repeat-until: _.size == 0
  trailer:
    seq: []