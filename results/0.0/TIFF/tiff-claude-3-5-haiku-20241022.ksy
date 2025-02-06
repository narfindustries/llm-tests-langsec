meta:
  id: tiff
  file-extension: tif
  endian: 
    switch-on: byte_order
    cases:
      'byte_order::little_endian': le
      'byte_order::big_endian': be

enums:
  byte_order:
    0x4949: little_endian
    0x4D4D: big_endian

  new_subfile_type:
    0x01: reduced_resolution
    0x02: page
    0x04: transparency_mask

  compression:
    1: none
    2: ccitt_group3_fax
    3: ccitt_group4_fax
    4: lzw
    5: jpeg
    6: jpeg_old
    32773: packbits

  photometric_interpretation:
    0: white_is_zero
    1: black_is_zero
    2: rgb
    3: palette_color
    4: transparency_mask
    5: cmyk
    6: ycbcr
    8: cielab

  orientation:
    1: top_left
    2: top_right
    3: bottom_right
    4: bottom_left
    5: left_top
    6: right_top
    7: right_bottom
    8: left_bottom

  planar_configuration:
    1: chunky
    2: planar

  resolution_unit:
    1: none
    2: inches
    3: centimeters

seq:
  - id: byte_order
    type: u2
    enum: byte_order
  - id: magic_number
    contents: [0x2A, 0x00]
  - id: first_ifd_offset
    type: u4
  - id: ifd
    type: image_file_directory
    repeat: until
    repeat-until: _.next_ifd_offset == 0

types:
  image_file_directory:
    seq:
      - id: num_fields
        type: u2
      - id: fields
        type: ifd_field
        repeat: expr
        repeat-expr: num_fields
      - id: next_ifd_offset
        type: u4

  ifd_field:
    seq:
      - id: tag
        type: u2
      - id: field_type
        type: u2
      - id: num_values
        type: u4
      - id: value_or_offset
        type: u4

  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4

  ascii_string:
    seq:
      - id: value
        type: str
        encoding: ASCII
        terminator: 0x00

  color_map:
    params:
      - id: bits_per_sample
        type: u2
    seq:
      - id: red_values
        type: u2
        repeat: expr
        repeat-expr: 1 << bits_per_sample
      - id: green_values
        type: u2
        repeat: expr
        repeat-expr: 1 << bits_per_sample
      - id: blue_values
        type: u2
        repeat: expr
        repeat-expr: 1 << bits_per_sample