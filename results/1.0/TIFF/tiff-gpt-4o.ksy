meta:
  id: tiff
  title: TIFF
  file-extension: tiff
  endian: be
  license: |
    This is a derived work, based on the TIFF 6.0 Specification.
    The Kaitai Struct definition itself is licensed under CC0.

seq:
  - id: header
    type: header

  - id: ifd0
    type: ifd
    doc: The first Image File Directory.

types:
  header:
    seq:
      - id: byte_order
        contents: ['I', 'I']
      - id: magic
        type: u2
      - id: ifd0_ofs
        type: u4

  ifd:
    seq:
      - id: num_tags
        type: u2
      - id: tags
        type: tag_entry
        repeat: expr
        repeat-expr: num_tags
      - id: next_ifd_ofs
        type: u4

  tag_entry:
    seq:
      - id: tag
        type: u2
        enum: tag_codes
      - id: field_type
        type: u2
        enum: field_types
      - id: length
        type: u4
      - id: value_or_offset
        type: u4

enums:
  tag_codes:
    256: image_width
    257: image_length
    258: bits_per_sample
    259: compression
    262: photometric_interpretation
    273: strip_offsets
    274: orientation
    277: samples_per_pixel
    278: rows_per_strip
    279: strip_byte_counts
    282: x_resolution
    283: y_resolution
    284: planar_configuration
    296: resolution_unit
    305: software
    306: date_time
    315: artist
    339: sample_format

  field_types:
    1: byte
    2: ascii
    3: short
    4: long
    5: rational
    6: sbyte
    7: undefined
    8: sshort
    9: slong
    10: srational
    11: float
    12: double