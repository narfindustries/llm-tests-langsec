meta:
  id: tiff
  title: Tagged Image File Format (TIFF)
  file-extension: tiff
  endian: le
  license: CC0-1.0
  encoding: UTF-8

seq:
  - id: header
    type: tiff_header

types:
  tiff_header:
    seq:
      - id: byte_order
        type: u2
        enum: byte_order
      - id: version
        type: u2
      - id: ifd_offset
        type: u4

  ifd_entry:
    seq:
      - id: tag
        type: u2
      - id: field_type
        type: u2
        enum: field_type
      - id: count
        type: u4
      - id: value_offset
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

enums:
  byte_order:
    0x4949: little_endian
    0x4D4D: big_endian

  field_type:
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