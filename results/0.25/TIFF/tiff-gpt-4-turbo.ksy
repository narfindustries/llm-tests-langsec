meta:
  id: tiff
  file-extension: tif
  endian: le
  title: TIFF (Tagged Image File Format)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  TIFF is a flexible, adaptable file format for handling images and data
  within a single file, by including the header tags (size, definition, image-data arrangement, applied image compression) defining the image's geometry.
  For example, a TIFF file can be a container holding compressed (lossy) JPEG and (lossless) PackBits compressed images.
  TIFF files also can include vector-based clipping paths (outlines, croppings, image frames).

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
        enum: tiff_byte_order
      - id: magic
        contents: [0x2a, 0x00]
      - id: offset_first_ifd
        type: u4

  ifd:
    seq:
      - id: num_tags
        type: u2
      - id: tags
        type: tag
        repeat: expr
        repeat-expr: num_tags
      - id: next_ifd_offset
        type: u4

    instances:
      next_ifd:
        pos: next_ifd_offset
        type: ifd
        if: next_ifd_offset != 0

  tag:
    seq:
      - id: tag_type
        type: u2
        enum: tag_type
      - id: tag_data_type
        type: u2
        enum: tag_data_type
      - id: num_values
        type: u4
      - id: value_offset
        type: u4

enums:
  tiff_byte_order:
    0x4949: le
    0x4d4d: be

  tag_type:
    0x0100: image_width
    0x0101: image_length
    0x0102: bits_per_sample
    0x0103: compression
    0x0106: photometric_interpretation
    0x010e: image_description
    0x0111: strip_offsets
    0x0115: samples_per_pixel
    0x0116: rows_per_strip
    0x0117: strip_byte_counts
    0x011a: x_resolution
    0x011b: y_resolution
    0x011c: planar_configuration
    0x0128: resolution_unit
    0x0131: software
    0x0132: datetime
    0x013b: artist
    0x0201: jpeg_interchange_format
    0x0202: jpeg_interchange_format_length
    0x0213: ycbcr_positioning
    0x8769: exif_ifd_pointer
    0x8825: gps_info_ifd_pointer

  tag_data_type:
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