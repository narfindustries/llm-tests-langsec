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
        enum: tiff_version
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
        enum: tiff_tag
      - id: field_type
        type: u2
        enum: tiff_field_type
      - id: count
        type: u4
      - id: value_offset
        type: u4
    instances:
      value:
        value: |
          field_type == tiff_field_type::byte ? value_offset :
          field_type == tiff_field_type::ascii ? value_offset :
          field_type == tiff_field_type::short ? value_offset :
          field_type == tiff_field_type::long ? value_offset :
          field_type == tiff_field_type::rational ? value_offset :
          field_type == tiff_field_type::sbyte ? value_offset :
          field_type == tiff_field_type::undefined ? value_offset :
          field_type == tiff_field_type::sshort ? value_offset :
          field_type == tiff_field_type::slong ? value_offset :
          field_type == tiff_field_type::srational ? value_offset :
          field_type == tiff_field_type::float ? value_offset :
          field_type == tiff_field_type::double ? value_offset :
          value_offset
enums:
  byte_order:
    0x4949: little_endian
    0x4d4d: big_endian
  tiff_version:
    42: classic
    43: big_tiff
  tiff_tag:
    254: new_subfile_type
    256: image_width
    257: image_length
    258: bits_per_sample
    259: compression
    262: photometric_interpretation
    263: thresholding
    264: cell_width
    265: cell_length
    266: fill_order
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
    306: datetime
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
    330: subifds
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
    344: xclip_path_units
    345: yclip_path_units
    346: indexed
    347: jpeg_tables
    351: opi_proxy
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
    530: ycbcr_sub_sampling
    531: ycbcr_positioning
    532: reference_black_white
    700: xmp
    33432: copyright
  tiff_field_type:
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