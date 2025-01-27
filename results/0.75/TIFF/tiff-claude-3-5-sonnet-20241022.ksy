meta:
  id: tiff
  file-extension: tif
  endian: le
  title: Tagged Image File Format
  application: Image File Format
  license: CC0-1.0

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
      - id: endian
        type: u2
        enum: endians
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
        enum: tags
      - id: field_type
        type: u2
        enum: field_types
      - id: count
        type: u4
      - id: data_offset
        type: u4

  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4

enums:
  endians:
    0x4949: le
    0x4D4D: be

  field_types:
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

  tags:
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
    326: bad_block_pointer
    327: bad_block_length
    328: bad_block_percent
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
    346: recorded_indexed
    347: jpeg_tables
    351: opi_proxy
    400: global_parameters
    401: profile_type
    402: fax_profile
    403: coding_methods
    404: version_year
    405: mode_number
    433: decode
    434: default_image_color
    512: jpeg_proc
    513: jpeg_interchange_format
    514: jpeg_interchange_format_length
    515: jpeg_restart_interval
    517: jpeg_lossless_predictors
    518: jpeg_point_transforms
    519: jpeg_q_tables
    520: jpeg_dc_tables
    521: jpeg_ac_tables
    529: y_cb_cr_coefficients
    530: y_cb_cr_sub_sampling
    531: y_cb_cr_positioning
    532: reference_black_white
    700: xml_packet
    32781: image_id
    32932: wang_annotation
    32934: page_at_zero
    32953: t88_options
    32995: thermal_palette
    32997: transparency_indicator
    32998: delay_for_next_image
    33432: copyright
    33434: exposure_time
    33437: f_number
    34377: photoshop
    34665: exif_ifd
    34675: icc_profile
    34853: gps_ifd
    40965: interoperability_ifd
    50255: h_p_private_data