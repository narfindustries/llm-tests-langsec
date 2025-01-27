meta:
  id: gif
  file-extension: gif
  endianness: le
  imports:
    - ../../commontraits.ksy
seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    size: if header.has_global_color_table == 1 then (2 << (header.color_resolution + 1)) * 3 else 0
    type: seq
    repeat: expr
    repeat-expr: 2 << (header.color_resolution + 1)
    seq:
      - id: rgb
        type: u1
      - id: r
        type: u1
      - id: g
        type: u1
      - id: b
        type: u1
  - id: images
    type: seq
    repeat: until struct.has_image == 0
    seq:
      - id: image
        type: image
types:
  header:
    seq:
      - id: magic
        content: "GIF"
      - id: version
        size: 3
        type: str
      - id: has_global_color_table
        type: u1
      - id: color_resolution
        type: u1
      - id: sorted
        type: u1
      - id: size_of_global_color_table
        type: u1
      - id: background_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
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
  image:
    seq:
      - id: separator
        content: "\x2c"
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
      - id: table_based_image_data
        type: table_based_image_data
  table_based_image_data:
    seq:
      - id: min_code_size
        type: u1
      - id: sub_blocks
        type: seq
        repeat: until struct.is_zero == 1
        seq:
          - id: length
            type: u1
          - id: data
            size: length
            type: u1
          - id: is_zero
            type: u1