meta:
  id: gif-gemini-1
  title: GIF (Gemini 1.5 Flash)
  homepage: https://kaitai.io
  file-extension: gif
  experimental: true

types:
  header:
    seq:
      - id: signature
        type: str
        size: 3
      - id: version
        type: str
        size: 3
      - id: screen_descriptor
        type: screen_descriptor
      - id: global_color_table
        type: global_color_table
      - id: blocks
        type: blocks

  screen_descriptor:
    seq:
      - id: width
        type: u2
      - id: height
        type: u2
      - id: packed_fields
        type: u1
        docs: |
          Bits 0-3: Global color table flag (1=present, 0=absent)
          Bits 4-7: Color resolution (1-8)
      - id: background_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1

  global_color_table:
    seq:
      - id: size
        type: u1
        expr: (packed_fields & 0x07) + 1
      - id: entries
        type: color_table_entry
        repeat: expr
        expr: (1 << size)

  color_table_entry:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

  blocks:
    seq:
      - id: block_type
        type: u1
      - id: block_data
        type: block_data

  block_data:
    switch-on: block_type
    cases:
      - 0x2C:
          seq:
            - id: image_descriptor
              type: image_descriptor
            - id: image_data
              type: image_data
      - 0x21:
          seq:
            - id: extension_type
              type: u1
            - id: extension_data
              type: extension_data
      - 0x3B:
          seq:
            - id: trailer
              type: trailer

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
        docs: |
          Bits 0-3: Local color table flag (1=present, 0=absent)
          Bits 4-7: Interlace flag (1=interlaced, 0=non-interlaced)
      - id: local_color_table
        type: local_color_table

  local_color_table:
    seq:
      - id: size
        type: u1
        expr: (packed_fields & 0x07) + 1
      - id: entries
        type: color_table_entry
        repeat: expr
        expr: (1 << size)

  image_data:
    seq:
      - id: lzw_minimum_code_size
        type: u1
      - id: data
        type: data_subblocks

  extension_data:
    seq:
      - id: subblocks
        type: data_subblocks

  trailer:
    seq:
      - id: end_code
        type: u1
        enum:
          0x3B: end

  data_subblocks:
    seq:
      - id: subblock_size
        type: u1
      - id: subblock_data
        type: bytes
        size: subblock_size
      - id: next_subblock
        type: data_subblocks
        repeat: expr
        expr: subblock_size != 0


