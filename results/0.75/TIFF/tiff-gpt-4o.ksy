meta:
  id: tiff
  title: TIFF (Tagged Image File Format)
  file-extension: tiff
  endian: le

seq:
  - id: byte_order
    type: u2
  - id: version
    type: u2
  - id: ifd0_ofs
    type: u4

instances:
  ifd0:
    pos: ifd0_ofs
    type: ifd

types:
  ifd:
    seq:
      - id: num_entries
        type: u2
      - id: entries
        type: ifd_entry
        repeat: expr
        repeat-expr: num_entries
      - id: next_ifd_ofs
        type: u4

  ifd_entry:
    seq:
      - id: tag
        type: u2
        enum: tag
      - id: field_type
        type: u2
        enum: field_type
      - id: num_values
        type: u4
      - id: value_or_ofs
        type: u4

enums:
  tag:
    0x0100: image_width
    0x0101: image_length
    0x0102: bits_per_sample
    0x0103: compression
    0x0106: photometric_interpretation
    0x0111: strip_offsets
    0x0115: samples_per_pixel
    0x0116: rows_per_strip
    0x0117: strip_byte_counts
    0x011a: x_resolution
    0x011b: y_resolution
    0x0128: resolution_unit
    0x0131: software
    0x0132: date_time
    0x013b: artist
    0x013c: host_computer
    0x0140: color_map
    0x0153: sample_format
    0x0112: orientation
    0x011c: planar_configuration
    0x013d: predictor
    0x0142: tile_width
    0x0143: tile_length
    0x0144: tile_offsets
    0x0145: tile_byte_counts

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