meta:
  id: tiff
  title: TIFF (Tagged Image File Format)
  file-extension: tiff
  endian: le
  license: CC0-1.0

doc: |
  TIFF is a flexible, adaptable file format for handling images and data within a single file, by including the header tags (size, definition, image-data arrangement, applied image compression) defining the image's geometry. For example, a TIFF file can be a container holding compressed (lossy, lossless) or uncompressed images, vector-based clipping paths, text, metadata, and two-dimensional vector graphics.

seq:
  - id: header
    type: tiff_header
  - id: ifds
    type: ifd
    repeat: eos

types:
  tiff_header:
    seq:
      - id: byte_order
        type: u2
        enum: endianness
      - id: magic
        contents: [0x2A, 0x00]
      - id: ofs_ifd
        type: u4

  ifd:
    seq:
      - id: num_fields
        type: u2
      - id: fields
        type: ifd_field
        repeat: expr
        repeat-expr: num_fields
      - id: next_ifd_ofs
        type: u4
        if: next_ifd_ofs != 0

  ifd_field:
    seq:
      - id: tag
        type: u2
        enum: tag
      - id: type
        type: u2
        enum: field_type
      - id: length
        type: u4
      - id: value_offset
        type: u4
    instances:
      value:
        pos: value_offset
        type:
          switch-on: type
          cases:
            'field_type::u1': values_u1(length)
            'field_type::u2': values_u2(length)
            'field_type::u4': values_u4(length)
            'field_type::i4': values_i4(length)
            'field_type::r8': values_r8(length)

enums:
  endianness:
    0x4949: le
    0x4d4d: be

  tag:
    0x0100: image_width
    0x0101: image_length
    0x0102: bits_per_sample
    0x0103: compression
    0x0106: photometric_interpretation
    0x0111: strip_offsets
    0x0112: orientation
    0x0115: samples_per_pixel
    0x0116: rows_per_strip
    0x0117: strip_byte_counts
    0x011A: x_resolution
    0x011B: y_resolution
    0x011C: planar_configuration
    0x0128: resolution_unit
    0x0131: software
    0x0132: date_time
    0x013B: artist
    0x0140: color_map
    0x0152: extra_samples
    0x0153: sample_format
    0x0213: ycbcr_positioning
    0x0214: reference_black_white
    0x828D: cfa_repeat_pattern_dim
    0x828E: cfa_pattern
    0x8825: gps_info

  field_type:
    0x0001: u1
    0x0002: u2
    0x0003: u4
    0x0004: i4
    0x0005: r8

types:
  values_u1:
    params:
      - id: num_values
        type: u4
    seq:
      - id: values
        type: u1
        repeat: expr
        repeat-expr: num_values

  values_u2:
    params:
      - id: num_values
        type: u4
    seq:
      - id: values
        type: u2be
        repeat: expr
        repeat-expr: num_values

  values_u4:
    params:
      - id: num_values
        type: u4
    seq:
      - id: values
        type: u4be
        repeat: expr
        repeat-expr: num_values

  values_i4:
    params:
      - id: num_values
        type: u4
    seq:
      - id: values
        type: s4be
        repeat: expr
        repeat-expr: num_values

  values_r8:
    params:
      - id: num_values
        type: u4
    seq:
      - id: values
        type: r8
        repeat: expr
        repeat-expr: num_values // 2
    types:
      r8:
        seq:
          - id: numerator
            type: u4
          - id: denominator
            type: u4