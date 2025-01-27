meta:
  id: tiff
  file-extension: tiff
  endian: le
  title: TIFF (Tagged Image File Format)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  TIFF is a flexible, adaptable file format for handling images and data
  within a single file by including the header tags (size, definition, image-data arrangement, applied image compression) defining the image's geometry.
  For example, a TIFF file can be a container holding compressed (lossy) JPEG and (lossless) PackBits compressed images.
  TIFF files also can include multiple images, and each image in a TIFF file is called a "directory" because of the way it organizes the image data.

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
        valid:
          any-of: [0x4949, 0x4d4d]
      - id: magic
        type: u2
        valid:
          eq: 42
      - id: offset
        type: u4

  ifd:
    seq:
      - id: num_entries
        type: u2
      - id: entries
        type: entry
        repeat: expr
        repeat-expr: num_entries
      - id: next_ifd
        type: u4
    instances:
      has_next_ifd:
        value: next_ifd != 0
      next:
        pos: next_ifd
        type: ifd
        if: has_next_ifd

  entry:
    seq:
      - id: tag
        type: u2
        enum: tag_type
      - id: type
        type: u2
        enum: field_type
      - id: length
        type: u4
      - id: value_offset
        type: u4
    instances:
      value:
        io: _root._io
        pos: value_offset
        size: length * field_type_sizes[type]
        type:
          switch-on: type
          cases:
            'field_type::byte': u1
            'field_type::ascii': str
            'field_type::short': u2
            'field_type::long': u4
            'field_type::rational': rational

  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4

enums:
  endian:
    0x4949: le
    0x4d4d: be

  tag_type:
    0x0100: image_width
    0x0101: image_length
    0x0102: bits_per_sample
    0x0103: compression
    0x0106: photometric_interpretation
    0x0112: orientation
    0x0115: samples_per_pixel
    0x011C: planar_configuration
    0x0213: ycbcr_positioning
    0x8769: exif_ifd

  field_type:
    1: byte
    2: ascii
    3: short
    4: long
    5: rational

instances:
  field_type_sizes:
    value:
      1: 1
      2: 1
      3: 2
      4: 4
      5: 8