meta:
  id: tiff
  file-extension: tiff
  endian: le

seq:
  - id: header
    type: header
  - id: ifd0
    type: ifd
    
types:
  header:
    seq:
      - id: endianness
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
        type: ifd_field
        repeat: expr
        repeat-expr: num_fields
      - id: next_ifd_offset
        type: u4

  ifd_field:
    seq:
      - id: tag
        type: u2
      - id: field_type
        type: u2
      - id: count
        type: u4
      - id: value_offset
        type: u4

    enums:
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

      tags:
        254: new_subfile_type
        255: subfile_type
        256: image_width
        257: image_length
        258: bits_per_sample
        259: compression
        262: photometric_interpretation
        270: image_description
        271: make
        272: model
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
        306: datetime
        315: artist
        338: extra_samples