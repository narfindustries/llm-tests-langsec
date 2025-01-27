meta:
  id: gif-gemini-1
  title: GIF Gemini 1.5 Flash
  endian: be

seq:
  - id: header
    type: gif_header
  - id: logical_screen_descriptor
    type: gif_logical_screen_descriptor
  - id: global_color_table
    type: gif_global_color_table
  - id: blocks
    type: blocks

types:
  gif_header:
    seq:
      - id: signature
        type: str
        size: 3
      - id: version
        type: str
        size: 3

  gif_logical_screen_descriptor:
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

  gif_global_color_table:
    seq:
      - id: color_table
        type: color_table

  color_table:
    seq:
      - id: entries
        type: color_table_entry
        repeat: expr
        repeat-expr: (self.parent.parent.logical_screen_descriptor.packed_fields & 0x80) != 0 ? (1 << ((self.parent.parent.logical_screen_descriptor.packed_fields & 0x07) + 1)) : 0

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
        until-expr: lambda x: x.terminator == 0x00

  block:
    seq:
      - id: size
        type: u1
      - id: data
        type: u1
        repeat: expr
        repeat-expr: self.size
      - id: terminator
        type: u1

