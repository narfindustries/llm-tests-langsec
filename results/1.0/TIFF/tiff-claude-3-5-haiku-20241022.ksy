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

  compression_type:
    1: no_compression
    2: ccitt_group3_fax
    3: ccitt_group4_fax
    4: lzw
    5: jpeg
    6: jpeg_new

  photometric_interpretation:
    0: white_is_zero
    1: black_is_zero
    2: rgb
    3: palette_color
    4: transparency_mask

  orientation:
    1: top_left
    2: top_right
    3: bottom_right
    4: bottom_left
    5: left_top
    6: right_top
    7: right_bottom
    8: left_bottom

  resolution_unit:
    1: no_unit
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

  ifd:
    seq:
      - id: num_entries
        type: u2
      - id: entries
        type: ifd_entry
        repeat: expr
        repeat-expr: num_entries
      - id: next_ifd_offset
        type: u4

seq:
  - id: byte_order
    type: u2
    enum: byte_order
  - id: version
    contents: [0x2A, 0x00]
  - id: first_ifd_offset
    type: u4
  - id: ifd_list
    type: ifd
    repeat: eos

instances:
  baseline_tags:
    type: str
    value: '0x0100, 0x0101, 0x0102, 0x0103, 0x0106, 0x0111, 0x0112, 0x0115, 0x0116, 0x0117, 0x011A, 0x011B, 0x0128'

  extended_tags:
    type: str
    value: '0x013B, 0x0102, 0x010E, 0x0131, 0x0132, 0x8769, 0x8825, 0x0140'

  tag_map:
    type: str
    value: |
      {
        "0x0100": "ImageWidth",
        "0x0101": "ImageLength",
        "0x0102": "BitsPerSample",
        "0x0103": "Compression",
        "0x0106": "PhotometricInterpretation",
        "0x0111": "StripOffsets",
        "0x0112": "Orientation",
        "0x0115": "SamplesPerPixel", 
        "0x0116": "RowsPerStrip",
        "0x0117": "StripByteCounts", 
        "0x011A": "XResolution",
        "0x011B": "YResolution",
        "0x0128": "ResolutionUnit"
      }