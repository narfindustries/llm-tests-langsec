meta:
  id: gif
  title: CompuServe Graphics Interchange Format
  application: GIF
  file-extension:
    - gif
  xref:
    forensicswiki: GIF
    wikidata: Q230853
  license: CC0-1.0
  endian: le

doc: |
  GIF is a bitmap image format that was introduced by CompuServe in 1987 and
  has since gained widespread usage on the World Wide Web due to its wide
  support and portability.

seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: color_table
    if: header.has_global_color_table
  - id: blocks
    type: block
    repeat: until
    repeat-until: _.block_type == block_types::end_of_file

types:
  header:
    seq:
      - id: signature
        contents: "GIF"
      - id: version
        size: 3
        type: str
      - id: width
        type: u2
      - id: height
        type: u2
      - id: flags
        type: u1
      - id: bg_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
    instances:
      has_global_color_table:
        value: (flags & 0x80) != 0

  logical_screen_descriptor:
    seq:
      - id: width
        type: u2
      - id: height
        type: u2
      - id: flags
        type: u1
      - id: background_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1

  color_table:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: (1 << ((flags & 0x07) + 1))

  rgb:
    seq:
      - id: r
        type: u1
      - id: g
        type: u1
      - id: b
        type: u1

  block:
    seq:
      - id: block_type
        type: u1
      - id: body
        size: (len - 1)
        type:
          switch-on: block_type
          cases:
            "0x21": extension_block
            "0x2c": image_descriptor
            "0x3b": end_of_file

  extension_block:
    seq:
      - id: label
        type: u1
      - id: block_size
        type: u1
      - id: extension_data
        size: block_size
        type: bytes

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
      - id: flags
        type: u1
      - id: local_color_table
        type: color_table
        if: flags & 0x80 != 0
      - id: lzw_minimum_code_size
        type: u1
      - id: image_data
        type: sub_blocks

  sub_blocks:
    seq:
      - id: sub_blocks
        type: bytes
        repeat: until
        repeat-until: subtype == 0x00

  end_of_file:
    seq: []