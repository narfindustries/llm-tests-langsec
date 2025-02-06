meta:
  id: tiff
  file-extension: tif
  endian: le
  title: Tagged Image File Format (TIFF)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  TIFF, short for Tagged Image File Format, is a variable, flexible, and adaptable file format for handling images and data within a single file. It supports various types of compression, color formats, and resolutions.

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
        enum: endian
        doc: Byte order used in the file (little or big endian).

      - id: magic
        type: u2
        valid:
          eq: 42
        doc: Magic number (42).

      - id: offset_first_ifd
        type: u4
        doc: Offset to the first IFD.

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

    instances:
      next_ifd:
        pos: next_ifd_offset
        type: ifd
        if: next_ifd_offset != 0

  ifd_entry:
    seq:
      - id: tag
        type: u2
        enum: tag_type

      - id: type
        type: u2
        enum: field_type

      - id: num_values
        type: u4

      - id: value_offset
        type: u4

    instances:
      values:
        pos: value_offset
        type:
          switch-on: type
          cases:
            'field_type::byte': bytes
            'field_type::ascii': strz
            'field_type::short': shorts
            'field_type::long': longs
            'field_type::rational': rationals

enums:
  endian:
    0x4949: le
    0x4d4d: be

  tag_type:
    256: image_width
    257: image_length
    258: bits_per_sample
    259: compression
    262: photometric_interpretation
    274: orientation
    277: samples_per_pixel
    278: rows_per_strip
    273: strip_offsets
    279: strip_byte_counts
    282: x_resolution
    283: y_resolution
    284: planar_configuration
    296: resolution_unit
    # Add other tags as necessary

  field_type:
    1: byte
    2: ascii
    3: short
    4: long
    5: rational
    # Add other types as necessary

types:
  bytes:
    seq:
      - id: data
        type: u1
        repeat: expr
        repeat-expr: _parent.num_values

  strz:
    seq:
      - id: str
        type: str
        encoding: ASCII
        size: _parent.num_values

  shorts:
    seq:
      - id: data
        type: u2
        repeat: expr
        repeat-expr: _parent.num_values

  longs:
    seq:
      - id: data
        type: u4
        repeat: expr
        repeat-expr: _parent.num_values

  rationals:
    seq:
      - id: data
        type: rational
        repeat: expr
        repeat-expr: _parent.num_values

  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4

    instances:
      value:
        value: numerator / denominator
        if: denominator != 0