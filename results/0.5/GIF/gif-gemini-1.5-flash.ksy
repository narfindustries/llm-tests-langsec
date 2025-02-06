meta:
  id: gif
  endian: be
seq:
  - id: signature
    type: str
    len: 6
    enum:
      GIF87a: "GIF87a"
      GIF89a: "GIF89a"
  - id: logical_screen_descriptor
    struct:
      - id: width
        type: u2
      - id: height
        type: u2
      - id: packed_fields
        type: u1
        struct:
          - id: global_color_table_flag
            type: bits
            size: 1
          - id: color_resolution
            type: bits
            size: 3
          - id: sort_flag
            type: bits
            size: 1
          - id: size_of_global_color_table
            type: bits
            size: 3
      - id: background_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
  - id: global_color_table
    seq:
      - type: u1
      - type: u1
      - type: u1
    size: (lambda this: (1 << (this.logical_screen_descriptor.packed_fields.size_of_global_color_table + 1)) * 3)
    if: this.logical_screen_descriptor.packed_fields.global_color_table_flag == 1
  - id: image_descriptors
    seq:
      - id: image_separator
        type: u1
        eq: 44
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
        struct:
          - id: local_color_table_flag
            type: bits
            size: 1
          - id: interlace_flag
            type: bits
            size: 1
          - id: sort_flag
            type: bits
            size: 1
          - id: size_of_local_color_table
            type: bits
            size: 3
      - id: local_color_table
        seq:
          - type: u1
          - type: u1
          - type: u1
        size: (lambda this: (1 << (this.packed_fields.size_of_local_color_table + 1)) * 3)
        if: this.packed_fields.local_color_table_flag == 1
      - id: lzw_minimum_code_size
        type: u1
      - id: image_data
        type: bytes
  - id: extensions
    seq:
      - id: extension_introducer
        type: u1
        eq: 33
      - id: extension_label
        type: u1
      - id: extension_data
        type: bytes
        read: (lambda this: this.read_until(lambda x: x == 0))
  - id: trailer
    type: u1
    eq: 59

