meta:
  id: tiff
  file-extension: tif
  endian: little

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
      - id: magic
        contents: [0x2A, 0x00]
      - id: first_ifd_offset
        type: u4

  ifd:
    seq:
      - id: num_fields
        type: u2
      - id: fields
        type: field
        repeat: expr
        repeat-expr: num_fields
      - id: next_ifd
        type: u4

  field:
    seq:
      - id: tag
        type: u2
        enum: tag
      - id: field_type
        type: u2
        enum: type
      - id: num_values
        type: u4
      - id: value_or_offset
        type: u4

enums:
  endian:
    0x4949: little
    0x4D4D: big

  tag:
    0x0100: image_width
    0x0101: image_length
    0x0102: bits_per_sample
    0x0103: compression
    0x0106: photometric_interpretation
    0x010F: make
    0x0110: model

  type:
    1: byte
    2: ascii
    3: short
    4: long
    5: rational