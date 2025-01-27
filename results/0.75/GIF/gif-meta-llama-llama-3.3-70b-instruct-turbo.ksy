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
    type: global_color_table
    if: logical_screen_descriptor.global_color_table_flag == 1
  - id: images
    type: image
    repeat: eos
types:
  header:
    seq:
      - id: signature
        contents: 'GIF'
      - id: version
        size: 3
  logical_screen_descriptor:
    seq:
      - id: width
        size: 2
      - id: height
        size: 2
      - id: flags
        size: 1
      - id: background_color_index
        size: 1
      - id: pixel_aspect_ratio
        size: 1
    instances:
      global_color_table_flag:
        value: flags & 128 != 0
  global_color_table:
    seq:
      - id: colors
        type: color
        repeat: expr
        count: 3 * (1 << (logical_screen_descriptor.flags & 7))
  color:
    seq:
      - id: red
        size: 1
      - id: green
        size: 1
      - id: blue
        size: 1
  image:
    seq:
      - id: descriptor
        type: image_descriptor
      - id: data
        type: image_data
  image_descriptor:
    seq:
      - id: left
        size: 2
      - id: top
        size: 2
      - id: width
        size: 2
      - id: height
        size: 2
      - id: flags
        size: 1
    instances:
      local_color_table_flag:
        value: flags & 128 != 0
      interlaced:
        value: flags & 64 != 0
  image_data:
    seq:
      - id: len
        size: 1
      - id: data
        size: len
      - id: terminator
        size: 1
        contents: 0