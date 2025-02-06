meta:
  id: tiff
  file-extension: tiff
  endian: le
seq:
  - id: header
    type: header
  - id: first_ifd
    type: ifd
types:
  header:
    seq:
      - id: byte_order
        type: u2
        enum: endian
      - id: version
        contents: [0x2A, 0x00]
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
        enum: tag_enum
      - id: field_type
        type: u2
        enum: field_type_enum
      - id: count
        type: u4
      - id: value_offset
        type: u4
    instances:
      value:
        io: _root._io
        pos: value_offset
        if: value_offset >= 4
        size: count * type_size
        type:
          switch-on: field_type
          cases:
            'field_type_enum::byte': u1
            'field_type_enum::ascii': str_ascii
            'field_type_enum::short': u2
            'field_type_enum::long': u4
            'field_type_enum::rational': rational
            'field_type_enum::sbyte': s1
            'field_type_enum::undefined': u1
            'field_type_enum::sshort': s2
            'field_type_enum::slong': s4
            'field_type_enum::srational': srational
            'field_type_enum::float': f4
            'field_type_enum::double': f8
      type_size:
        value: >-
          field_type == field_type_enum::byte ? 1 :
          field_type == field_type_enum::ascii ? 1 :
          field_type == field_type_enum::short ? 2 :
          field_type == field_type_enum::long ? 4 :
          field_type == field_type_enum::rational ? 8 :
          field_type == field_type_enum::sbyte ? 1 :
          field_type == field_type_enum::undefined ? 1 :
          field_type == field_type_enum::sshort ? 2 :
          field_type == field_type_enum::slong ? 4 :
          field_type == field_type_enum::srational ? 8 :
          field_type == field_type_enum::float ? 4 :
          field_type == field_type_enum::double ? 8 : 0
  str_ascii:
    seq:
      - id: data
        type: str
        size-eos: true
        encoding: ASCII
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
    0x4d4d: be
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
    326: bad_fax_lines
    327: clean_fax_data
    328: consecutive_bad_fax_lines
    330: sub_ifds
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
    343: clip_path
    344: x_clip_path_units
    345: y_clip_path_units
    346: indexed
    347: jpeg_tables
    351: ycbcr_coefficients
    352: ycbcr_sub_sampling
    353: ycbcr_positioning
    354: reference_black_white
    355: strip_row_counts
    357: reference_strip_counts
    433: copyright
    512: jpeg_proc
    513: jpeg_interchange_format
    514: jpeg_interchange_format_length
    515: jpeg_restart_interval
    517: jpeg_lossless_predictors
    518: jpeg_point_transforms
    519: jpeg_q_tables
    520: jpeg_dc_tables
    521: jpeg_ac_tables