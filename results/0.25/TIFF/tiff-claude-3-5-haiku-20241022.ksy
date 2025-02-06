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
    0x00: full_resolution
    0x01: reduced_resolution
    0x02: single_page
    0x04: transparency_mask

  compression:
    1: no_compression
    2: ccitt_group3_fax
    3: ccitt_group4_fax
    4: lzw
    5: jpeg
    6: jpeg_old
    7: jpeg_new
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

types:
  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4

  ifd_entry:
    seq:
      - id: tag
        type: u2
      - id: field_type
        type: u2
      - id: count
        type: u4
      - id: value_or_offset
        type: u4

  ifd_data:
    seq:
      - id: num_entries
        type: u2
      - id: entries
        type: ifd_entry
        repeat: expr
        repeat-expr: num_entries
      - id: next_ifd
        type: u4
    instances:
      new_subfile_type:
        value: >-
          entries[entries.tag == 0x00FE].value_or_offset
        enum: new_subfile_type
      image_width:
        value: >-
          entries[entries.tag == 0x0100].value_or_offset
      image_length:
        value: >-
          entries[entries.tag == 0x0101].value_or_offset
      bits_per_sample:
        value: >-
          entries[entries.tag == 0x0102].value_or_offset
      compression:
        value: >-
          entries[entries.tag == 0x0103].value_or_offset
        enum: compression
      photometric_interpretation:
        value: >-
          entries[entries.tag == 0x0106].value_or_offset
        enum: photometric_interpretation
      strip_offsets:
        value: >-
          entries[entries.tag == 0x0111].value_or_offset
      orientation:
        value: >-
          entries[entries.tag == 0x0112].value_or_offset
        enum: orientation
      samples_per_pixel:
        value: >-
          entries[entries.tag == 0x0115].value_or_offset
      rows_per_strip:
        value: >-
          entries[entries.tag == 0x0116].value_or_offset
      strip_byte_counts:
        value: >-
          entries[entries.tag == 0x0117].value_or_offset
      x_resolution:
        value: >-
          entries[entries.tag == 0x011A].value_or_offset
        type: rational
      y_resolution:
        value: >-
          entries[entries.tag == 0x011B].value_or_offset
        type: rational
      planar_configuration:
        value: >-
          entries[entries.tag == 0x011C].value_or_offset
        enum: planar_configuration
      resolution_unit:
        value: >-
          entries[entries.tag == 0x0128].value_or_offset
        enum: resolution_unit

seq:
  - id: byte_order
    type: u2
    enum: byte_order
  - id: magic_number
    contents: [0x2A, 0x00]
  - id: first_ifd_offset
    type: u4
  - id: ifd
    type: ifd_data
    size: _io.size - _io.pos