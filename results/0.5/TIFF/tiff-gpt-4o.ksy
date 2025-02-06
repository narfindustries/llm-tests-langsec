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
        contents: ['I', 'I']
        doc: Byte order indication (II for little-endian, MM for big-endian)
      - id: version
        type: u2
        doc: TIFF version number (should be 42)
      - id: ifd0_offset
        type: u4
        doc: Offset to the first Image File Directory (IFD)

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
      - id: type
        type: u2
        enum: field_type_enum
      - id: count
        type: u4
      - id: value_offset
        type: u4

enums:
  tag_enum:
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
    320: color_map
    338: extra_samples
    339: sample_format
    322: tile_width
    323: tile_length
    324: tile_offsets
    325: tile_byte_counts
    317: predictor

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