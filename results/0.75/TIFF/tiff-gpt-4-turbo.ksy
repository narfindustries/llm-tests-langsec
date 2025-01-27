meta:
  id: tiff
  title: TIFF (Tagged Image File Format)
  file-extension: tiff, tif
  endian: le
  license: CC0-1.0
  ks-version: 0.9

doc: |
  TIFF, Tagged Image File Format, is a file format that is widely used
  for storing raster graphics and images as well as vector images.

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
        doc: Byte order used in the file (little or big endian).

      - id: magic
        contents: [0x002a]
        doc: Magic number (42).

      - id: offset
        type: u4
        doc: Offset to the first IFD.

  ifd:
    seq:
      - id: num_of_entries
        type: u2

      - id: entries
        type: ifd_entry
        repeat: expr
        repeat-expr: num_of_entries

      - id: next_ifd
        type: u4
        doc: Offset to the next IFD.

  ifd_entry:
    seq:
      - id: tag
        type: u2
        enum: tag_type

      - id: type
        type: u2
        enum: field_type

      - id: length
        type: u4

      - id: offset
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
    0x010e: image_description
    0x010f: make
    0x0110: model
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