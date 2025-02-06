meta:
  id: tiff
  file-extension: tiff
  endian: le
  title: Tagged Image File Format (TIFF)
seq:
  - id: header
    type: header
  - id: ifds
    type: ifd
    repeat: eos

types:
  header:
    seq:
      - id: byte_order
        type: u2
        enum: endianness
      - id: magic
        contents: [0x2A, 0x00]
      - id: offset_of_ifd
        type: u4
  ifd:
    seq:
      - id: num_of_entries
        type: u2
      - id: entries
        type: entry
        repeat: expr
        repeat-expr: num_of_entries
      - id: next_ifd_offset
        type: u4
    types:
      entry:
        seq:
          - id: tag
            type: u2
            enum: tag_type
          - id: field_type
            type: u2
            enum: field_type
          - id: length
            type: u4
          - id: value_offset
            type: u4
enums:
  endianness:
    0x4949: little
    0x4d4d: big
  tag_type:
    0x0100: image_width
    0x0101: image_length
    0x0102: bits_per_sample
    0x0103: compression
    0x0106: photometric_interpretation
    0x0111: strip_offsets
    0x0115: samples_per_pixel
    0x0116: rows_per_strip
    0x0117: strip_byte_counts
    0x011a: x_resolution
    0x011b: y_resolution
    0x011c: planar_configuration
    0x0128: resolution_unit
    0x0131: software
    0x0132: date_time
    0x013b: artist
    0x013e: white_point
    0x013f: primary_chromaticities
    0x0211: ycbcr_coefficients
    0x0213: ycbcr_positioning
    0x0214: reference_black_white
    0x8769: exif_ifd
    0x8825: gps_ifd
  field_type:
    1: byte
    2: ascii
    3: short
    4: long
    5: rational
    6: signed_long
    7: undefined
    8: signed_byte
    9: signed_short
    10: signed_rational
    11: float
    12: double