meta:
  id: gif
  endian: be
seq:
  - id: signature
    type: str
    len: 6
    encoding: ASCII
    matches: "GIF89a"
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
            len: 1
          - id: color_resolution
            type: bits
            len: 3
          - id: sort_flag
            type: bits
            len: 1
          - id: size_of_global_color_table
            type: bits
            len: 3
      - id: background_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
  - id: global_color_table
    seq:
      - id: color_table_entry
        type: u3
    size: (1 << ((this.logical_screen_descriptor.packed_fields.size_of_global_color_table + 1)))
    if: this.logical_screen_descriptor.packed_fields.global_color_table_flag == 1
  - id: image_descriptors
    seq:
      - id: image_separator
        type: u1
        matches: 0x2C
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
            len: 1
          - id: interlace_flag
            type: bits
            len: 1
          - id: sort_flag
            type: bits
            len: 1
          - id: reserved
            type: bits
            len: 2
          - id: size_of_local_color_table
            type: bits
            len: 3
      - id: local_color_table
        seq:
          - id: color_table_entry
            type: u3
        size: (1 << ((this.packed_fields.size_of_local_color_table + 1)))
        if: this.packed_fields.local_color_table_flag == 1
      - id: lzw_min_code_size
        type: u1
      - id: lzw_data
        type: bytes
        size: this.image_width * this.image_height
  - id: extension_blocks
    seq:
      - id: extension_introducer
        type: u1
        matches: 0x21
      - id: extension_label
        type: u1
      - id: extension_data
        type: bytes
        read: until_terminator(0x00)
  - id: trailer
    type: u1
    matches: 0x3B

