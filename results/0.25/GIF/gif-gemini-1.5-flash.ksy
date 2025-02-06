type: struct
id: gif
endian: be

seq:
  - id: signature
    type: str
    len: 6
    encoding: ASCII
    assert: signature == "GIF89a"
  - id: logical_screen_descriptor
    type: struct
    fields:
      - id: width
        type: u2
      - id: height
        type: u2
      - id: packed_fields
        type: u1
        fields:
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
    type: seq
    size: lambda: (1 << (self.logical_screen_descriptor.packed_fields.size_of_global_color_table + 1)) if self.logical_screen_descriptor.packed_fields.global_color_table_flag else 0
    type: struct
    fields:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1
  - id: blocks
    type: seq
    repeat: until
    until: lambda: self._io.is_eof()
    type: gif_block

types:
  gif_block:
    type: struct
    fields:
      - id: block_type
        type: u1
      - id: block_data
        type: bytes
        size: lambda: self.block_type

  gif_image_descriptor:
    type: struct
    fields:
      - id: image_separator
        type: u1
        assert: image_separator == 0x2C
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
        fields:
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
        type: seq
        size: lambda: (1 << (self.packed_fields.size_of_local_color_table + 1)) if self.packed_fields.local_color_table_flag else 0
        type: struct
        fields:
          - id: red
            type: u1
          - id: green
            type: u1
          - id: blue
            type: u1
      - id: lzw_minimum_code_size
        type: u1
      - id: image_data
        type: gif_data_blocks

  gif_data_blocks:
    type: seq
    repeat: until
    until: lambda: self._io.read_u1() == 0
    type: struct
    fields:
      - id: block_size
        type: u1
      - id: block_data
        type: bytes
        size: lambda: self.block_size

  gif_extension_block:
    type: struct
    fields:
      - id: extension_introducer
        type: u1
        assert: extension_introducer == 0x21
      - id: extension_label
        type: u1
      - id: extension_data
        type: gif_data_blocks

