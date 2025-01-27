meta:
  id: gif
  file-extension: gif
  title: GIF (Graphics Interchange Format)
  endianness: le
seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: data_blocks
    type: data_blocks
    repeat: expr
    until: (_io.pos >= _io.size)
types:
  header:
    seq:
      - id: magic
        content: ['G', 'I', 'F']
      - id: version
        size: 3
      - id: reserved
        size: 1
      - id: color_table_size
        size: 1
  logical_screen_descriptor:
    seq:
      - id: width
        size: 2
      - id: height
        size: 2
      - id: flags
        size: 1
      - id: bg_color_index
        size: 1
      - id: pixel_aspect_ratio
        size: 1
  data_blocks:
    seq:
      - id: block_type
        size: 1
      - id: block_data
        type: block_data
  block_data:
    seq:
      - id: size
        size: 1
      - id: data
        size: size
        if: size > 0