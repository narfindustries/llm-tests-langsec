meta:
  id: tiff
  file-extension: tiff
  application: image/tiff
  endian: le

seq:
  - id: header
    type: header
  - id: ifd0
    type: ifd
    doc: First IFD (main image)

types:
  header:
    seq:
      - id: endianness
        contents: [0x49, 0x49]
      - id: version
        contents: [0x2A, 0x00]
      - id: ifd_offset
        type: u4
        doc: Offset to first IFD

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

  ifd_entry:
    seq:
      - id: tag
        type: u2
        enum: tag_enum
      - id: field_type
        type: u2
        enum: field_type_enum
      - id: count
        type: u4
      - id: value_or_offset
        type: u4

    enums:
      tag_enum:
        0x0100: image_width
        0x0101: image_length
        0x0102: bits_per_sample
        0x0103: compression
        0x0106: photometric_interpretation
        0x0111: strip_offsets
        0x0115: samples_per_pixel
        0x0116: rows_per_strip
        0x0117: strip_byte_counts

      field_type_enum:
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