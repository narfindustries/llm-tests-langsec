meta:
  id: gif
  file-extension: gif
  endianness: le
  encoding: UTF-8
seq:
  - id: header
    type: header
  - id: logical_screen
    type: logical_screen
  - id: frames
    type: frame
    repeat: until_eof
types:
  header:
    seq:
      - id: magic
        content: 'GIF'
      - id: version
        size: 3
        encoding: ASCII
  logical_screen:
    seq:
      - id: width
        type: u2
      - id: height
        type: u2
      - id: flags
        type: u1
      - id: bg_color
        type: u1
      - id: pixel_aspect_ratio
        type: u1
  frame:
    seq:
      - id: descriptor
        type: frame_descriptor
      - id: pixels
        type: pixel_data
      - id: trailer
        content: '\x00'
    if: header.magic == 'GIF87a' || header.magic == 'GIF89a'
  frame_descriptor:
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
      - id: color_table_size
        type: u1
  pixel_data:
    seq:
      - id: min_code_size
        type: u1
      - id: data
        type: code_table
        size: eos
  code_table:
    seq:
      - id: code_size
        type: u1
      - id: values
        type: u1
        repeat: until code_size < values