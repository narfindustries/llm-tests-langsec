meta:
  id: gif-gemini-1
  title: GIF Gemini 1.5 Flash
  homepage: https://github.com/kaitai-io/kaitai_struct_formats
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
        instances:
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

  global_color_table:
    seq:
      - id: entries
        type: color_table_entry
        repeat: expr
        repeat-expr: (1 << (screen_descriptor.packed_fields.size_of_global_color_table + 1))

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
      - id: block
        type: block
        repeat: until
        repeat-until: block.type == 0x3b

  block:
    seq:
      - id: type
        type: u1
      - id: data
        type: data
        size: lambda this: this.length
      - id: length
        type: u1

