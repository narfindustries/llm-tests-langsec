meta:
  id: tiff
  file-extension: tiff
  endian: le

seq:
  - id: header
    type: header
  - id: ifds
    type: ifd
    repeat: until
    repeat-until: _.next_ifd_offset == 0

types:
  header:
    seq:
      - id: endianness
        type: u2
        enum: endian
      - id: version
        contents: [0x2A, 0x00]
      - id: ifd_offset
        type: u4

  ifd:
    seq:
      - id: num_fields
        type: u2
      - id: fields
        type: ifd_field
        repeat: expr
        repeat-expr: num_fields
      - id: next_ifd_offset
        type: u4

  ifd_field:
    seq:
      - id: tag
        type: u2
        enum: tag_enum
      - id: field_type
        type: u2
        enum: field_type_enum
      - id: count
        type: u4
      - id: value_or_offset
        type: u4

  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4

  srational:
    seq:
      - id: numerator
        type: s4
      - id: denominator
        type: s4

enums:
  endian:
    0x4949: le
    0x4D4D: be

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

  tag_enum:
    254: new_subfile_type
    255: subfile_type
    256: image_width
    257: image_length
    258: bits_per_sample
    259: compression
    262: photometric_interpretation
    263: threshholding
    264: cell_width
    265: cell_length
    266: fill_order
    269: document_name
    270: image_description
    271: make
    272: model
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
    286: x_position
    287: y_position
    288: free_offsets
    289: free_byte_counts
    290: gray_response_unit
    291: gray_response_curve
    292: t4_options
    293: t6_options
    296: resolution_unit
    297: page_number
    301: transfer_function
    305: software
    306: date_time
    315: artist
    316: host_computer
    317: predictor
    318: white_point
    319: primary_chromaticities
    320: color_map
    321: halftone_hints
    322: tile_width
    323: tile_length
    324: tile_offsets
    325: tile_byte_counts
    332: ink_set
    333: ink_names
    334: number_of_inks
    336: dot_range
    337: target_printer
    338: extra_samples
    339: sample_format
    340: s_min_sample_value
    341: s_max_sample_value
    342: transfer_range
    512: jpeg_proc
    513: jpeg_interchange_format
    514: jpeg_interchange_format_length
    515: jpeg_restart_interval
    517: jpeg_lossless_predictors
    518: jpeg_point_transforms
    519: jpeg_q_tables
    520: jpeg_dc_tables
    521: jpeg_ac_tables
    529: ycbcr_coefficients
    530: ycbcr_subsampling
    531: ycbcr_positioning
    532: reference_black_white
    33432: copyright