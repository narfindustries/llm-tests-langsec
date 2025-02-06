meta:
  id: tiff
  file-extension: tif
seq:
  - id: header
    type: tiff_header
types:
  tiff_header:
    seq:
      - id: byte_order
        type: byte_order
      - id: magic
        type: u2
      - id: ifd_offset
        type: u4
    types:
      byte_order:
        seq:
          - id: ii
            type: str
            size: 2
  tiff_ifd:
    seq:
      - id: num_entries
        type: u2
      - id: entries
        type: tiff_ifd_entry
        repeat: num_entries
      - id: next_ifd_offset
        type: u4
  tiff_ifd_entry:
    seq:
      - id: tag
        type: u2
      - id: type
        type: u2
      - id: num_values
        type: u4
      - id: value_offset
        type: u4
  u1:
    format: u1
  u2:
    format: u2
  u4:
    format: u4
  u8:
    format: u8
  s1:
    format: s1
  s2:
    format: s2
  s4:
    format: s4
  s8:
    format: s8
  f4:
    format: f4
  f8:
    format: f8
  str:
    type: string
    encoding: ASCII
  date_time:
    type: string
    encoding: ASCII
    format: YYYY:MM:DD HH:MM:SS
  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4
  ifd_value:
    type: 
      if: type == 1
        then: u1
        count: num_values
      elif: type == 2
        then: u1
        count: num_values
      elif: type == 3
        then: u2
        count: num_values
      elif: type == 4
        then: u4
        count: num_values
      elif: type == 5
        then: rational
        count: num_values
      elif: type == 7
        then: str
        count: num_values
      elif: type == 9
        then: s4
        count: num_values
      elif: type == 10
        then: rational
        count: num_values
      elif: type == 11
        then: str
        count: num_values
      else:
        type: u4
  image_width:
    seq:
      - id: width
        type: u4
  image_length:
    seq:
      - id: length
        type: u4
  bits_per_sample:
    seq:
      - id: bits_per_sample
        type: u2
  compression:
    seq:
      - id: compression
        type: u2
  photometric_interpretation:
    seq:
      - id: photometric_interpretation
        type: u2
  orientation:
    seq:
      - id: orientation
        type: u2
  samples_per_pixel:
    seq:
      - id: samples_per_pixel
        type: u2
  rows_per_strip:
    seq:
      - id: rows_per_strip
        type: u4
  strip_offsets:
    seq:
      - id: offset
        type: u4
  strip_byte_counts:
    seq:
      - id: strip_byte_counts
        type: u4
  planar_configuration:
    seq:
      - id: planar_configuration
        type: u2
  page_name:
    seq:
      - id: page_name
        type: str
type_mapping:
  256: image_width
  257: image_length
  258: bits_per_sample
  259: compression
  262: photometric_interpretation
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
  296: resolution_unit
  297: page_number
  301: transfer_function
  305: software
  306: date_time
  315: artist
  316: host_computer
  318: white_point
  319: primary_chromaticities
  320: color_map
  321: halftone_hints
  322: tile_width
  323: tile_length
  324: tile_offsets
  325: tile_byte_counts
  329: bad_fax_lines
  330: clean_fax_data
  331: consecutive_bad_fax_lines
  332: sub_ifds
  333: ink_set
  334: ink_names
  335: number_of_inks
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
  515: jpeg_raw_restart_interval
  520: jpeg_lossless_predictors
  521: jpeg_point_transforms
  522: jpeg_q_tables
  523: jpeg_dctables
  524: jpeg_actables
  529: ycb_cr_coefficients
  530: ycb_cr_sub_sampling
  531: ycb_cr_positioning
  532: reference_black_white
  559: frame_top
  560: frame_left
  561: frame_right
  562: frame_bottom
  569: broadcast_if_type
  570: image_source
  574: geo_tiff_directory
  599: pixel_intensity