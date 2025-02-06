meta:
  id: tiff
  title: TIFF
  file-extension: tiff
  endian: be

seq:
  - id: header
    type: header

types:
  header:
    seq:
      - id: byte_order
        type: u2
        enum: byte_order
      - id: version
        type: u2
      - id: ifd_offset
        type: u4

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
        enum: tag_type
      - id: field_type
        type: u2
        enum: field_type
      - id: num_values
        type: u4
      - id: value_offset
        type: u4

enums:
  byte_order:
    0x4949: le
    0x4d4d: be

  tag_type:
    256: image_width
    257: image_length
    258: bits_per_sample
    259: compression
    262: photometric_interpretation
    273: strip_offsets
    277: samples_per_pixel
    278: rows_per_strip
    279: strip_byte_counts
    282: x_resolution
    283: y_resolution
    284: planar_configuration
    296: resolution_unit
    305: software
    306: date_time
    315: artist
    316: host_computer
    320: color_map
    322: tile_width
    323: tile_length
    324: tile_offsets
    325: tile_byte_counts
    330: sub_ifds
    512: jpeg_proc
    513: jpeg_interchange_format
    514: jpeg_interchange_format_length
    529: ycbcr_coefficients
    530: ycbcr_sub_sampling
    531: ycbcr_positioning
    532: reference_black_white

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