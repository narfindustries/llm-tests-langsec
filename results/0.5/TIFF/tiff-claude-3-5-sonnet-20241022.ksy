meta:
  id: tiff
  file-extension: tiff
  endian: le

seq:
  - id: header
    type: header
  - id: ifd
    type: ifd
    repeat: until
    repeat-until: _.next_ifd_offset == 0

types:
  header:
    seq:
      - id: endian
        contents: [0x49, 0x49]
      - id: version
        contents: [0x2A, 0x00]
      - id: ifd_offset
        type: u4

  ifd:
    seq:
      - id: num_fields
        type: u2
      - id: fields
        type: field
        repeat: expr
        repeat-expr: num_fields
      - id: next_ifd_offset
        type: u4

  field:
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
        254: new_subfile_type
        256: image_width
        257: image_length
        258: bits_per_sample
        259: compression
        262: photometric_interpretation
        270: image_description
        273: strip_offsets
        277: samples_per_pixel
        278: rows_per_strip
        279: strip_byte_counts
        282: x_resolution
        283: y_resolution
        296: resolution_unit
        305: software
        306: datetime
        315: artist
        700: xmp

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