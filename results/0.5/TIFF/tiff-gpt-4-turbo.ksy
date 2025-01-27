meta:
  id: tiff
  file-extension: tiff
  endian: le
  title: TIFF (Tagged Image File Format)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  TIFF is a flexible, adaptable file format for handling images and data
  within a single file, by including the header tags (size, definition, image
  data arrangement, applied image compression) defining the image's geometry.
  For example, a TIFF file can be a container holding compressed (lossy) JPEG
  and (lossless) PackBits compressed images. TIFF files also support layers
  and multiple pages.

seq:
  - id: header
    type: header

  - id: ifd
    type: ifd
    repeat: eos

types:
  header:
    seq:
      - id: endianness
        type: u2
        enum: endianness_type
      - id: magic
        contents: [0x2A, 0x00]

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
        enum: tag_type
      - id: type
        type: u2
        enum: field_type
      - id: length
        type: u4
      - id: value_offset
        type: u4

enums:
  endianness_type:
    0x4949: le
    0x4D4D: be

  tag_type:
    0x0100: image_width
    0x0101: image_length
    0x0102: bits_per_sample
    0x0103: compression
    0x0106: photometric_interpretation
    0x0111: strip_offsets
    0x0115: samples_per_pixel
    0x0116: rows_per_strip
    0x0117: strip_byte_counts
    0x011A: x_resolution
    0x011B: y_resolution
    0x0128: resolution_unit
    0x0131: software
    0x0132: datetime
    0x8769: exif_ifd

  field_type:
    1: byte
    2: ascii
    3: short
    4: long
    5: rational
    6: signed_byte
    7: undefined
    8: signed_short
    9: signed_long
    10: signed_rational
    11: float
    12: double