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
      - id: count
        type: u4
      - id: value_offset
        type: u4
  byte_order:
    enums:
      0x4949: little
      0x4d4d: big
  tag_type:
    enums:
      254: subfile_type
      256: image_width
      257: image_length
      258: bits_per_sample
      259: compression
      262: photometric_interpretation
      263: thresholding
      264: cell_width
      265: cell_length
      266: fill_order
      269: document_name
      273: strip_offsets
      274: orientation
      277: samples_per_pixel
      278: rows_per_strip
      279: strip_byte_counts
      280: min_sample_value
      281: max_sample_value
      282: x_resolution
      283: y_resolution
      284: planar_configuration
      285: page_name
      290: gray_response_unit
      296: resolution_unit
      305: software
      306: date_time
      315: artist
      316: host_computer
      317: predictor
      320: color_map
      338: extra_samples
      339: sample_format
      347: jpeg_tables
      529: ycbcr_coefficients
      530: ycbcr_sub_sampling
      531: ycbcr_positioning
      532: reference_black_white
      33432: copyright
  field_type:
    enums:
      1: byte_type
      2: ascii_type
      3: short_type
      4: long_type
      5: rational_type
      6: sbyte_type
      7: undefined_type
      8: sshort_type
      9: slong_type
      10: srational_type
      11: float_type
      12: double_type