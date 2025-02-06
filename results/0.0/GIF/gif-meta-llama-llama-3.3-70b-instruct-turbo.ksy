meta:
  id: gif
  endian: le
seq:
  - id: signature
    type: str
    size: 3
    encoding: ascii
  - id: version
    type: str
    size: 3
    encoding: ascii
  - id: logical_screen_width
    type: u2
  - id: logical_screen_height
    type: u2
  - id: packed_fields
    type: u1
  - id: background_color_index
    type: u1
  - id: pixel_aspect_ratio
    type: u1
  - id: global_color_table
    type: seq
    repeat: expr
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1
    repeat_expr: (1 << ((packed_fields & 0x70) >> 4))
  - id: images
    type: seq
    repeat: until
    until: _io.eof()
    seq:
      - id: image_separator
        type: u1
        value: 44
      - id: image_left_position
        type: u2
      - id: image_top_position
        type: u2
      - id: image_width
        type: u2
      - id: image_height
        type: u2
      - id: image_packed_fields
        type: u1
      - id: local_color_table
        type: seq
        repeat_expr: (1 << ((image_packed_fields & 0x1f) + 1))
        seq:
          - id: red
            type: u1
          - id: green
            type: u1
          - id: blue
            type: u1
        if: image_packed_fields & 0x80 != 0
      - id: image_data
        type: seq
        repeat: until
        until: sub_block_size == 0
        seq:
          - id: sub_block_size
            type: u1
          - id: sub_block_data
            type: str
            size: sub_block_size
            encoding: none
  - id: trailer
    type: u1
    value: 59