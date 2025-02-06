meta:
  id: tiff
  file-extension: tiff
  endian: le
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
        enum: byte_orders
      - id: version
        type: u2
      - id: first_ifd_offset
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
        enum: tags
      - id: field_type
        type: u2
        enum: field_types
      - id: count
        type: u4
      - id: value_offset
        type: u4
  byte_orders:
    enums:
      18761: little_endian
      19789: big_endian
  field_types:
    enums:
      1: byte
      2: ascii
      3: short
      4: long
      5: rational
  tags:
    enums:
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
      338: extra_samples
      339: sample_format
      33432: copyright
      532: reference_black_white
      332: ink_set
      336: dot_range