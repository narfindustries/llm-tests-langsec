meta:
  id: tiff
  file-extension: tiff
  endian: little
seq:
  - id: header
    type: header
  - id: entries
    type: ifd_entry
    repeat: expr
    repeat-expr: header.num_entries
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
      - id: num_entries
        type: u2
  ifd_entry:
    seq:
      - id: tag
        type: u2
        enum: tag_type
      - id: field_type
        type: u2
        enum: field_type
      - id: num_values
        type: u4
      - id: value_or_offset
        type: u4
enums:
  endian:
    0x4949: little
    0x4D4D: big
  tag_type:
    0x0100: image_width
    0x0101: image_length
    0x0102: bits_per_sample
    0x0103: compression
    0x0106: photometric_interpretation
  field_type:
    1: byte
    2: ascii
    3: short
    4: long
    5: rational