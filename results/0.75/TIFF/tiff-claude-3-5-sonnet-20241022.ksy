meta:
  id: tiff
  file-extension: tif
  endian: le
seq:
  - id: header
    type: header
types:
  header:
    seq:
      - id: byte_order
        type: u2
        enum: endian
      - id: version
        type: u2
      - id: ifd_offset
        type: u4
    instances:
      is_le:
        value: byte_order == endian::little_endian
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
  ifd:
    seq:
      - id: num_fields
        type: u2
      - id: entries
        type: ifd_entry
        repeat: expr
        repeat-expr: num_fields
      - id: next_ifd_offset
        type: u4
enums:
  endian:
    0x4949: little_endian
    0x4d4d: big_endian
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
    290: gray_response_unit
    291: gray_response_curve
    292: group3_options
    293: group4_options
    296: resolution_unit
    305: software
    306: date_time
    315: artist
    316: host_computer
    320: color_map
    512: jpeg_proc
    513: jpeg_interchange_format
    514: jpeg_interchange_format_length
    515: jpeg_restart_interval
    519: jpeg_qtables
    520: jpeg_dctables
    521: jpeg_actables
    529: ycbcr_coefficients
    530: ycbcr_subsampling
    531: ycbcr_positioning
    532: reference_black_white
    33432: copyright
  compression:
    1: none
    2: ccitt_group3_1d
    3: ccitt_group3_2d
    4: ccitt_group4
    5: lzw
    32773: packbits
  photometric:
    0: white_is_zero
    1: black_is_zero
    2: rgb
    3: palette_color
    4: transparency_mask
    6: ycbcr
  orientation:
    1: top_left
    2: top_right
    3: bottom_right
    4: bottom_left
    5: left_top
    6: right_top
    7: right_bottom
    8: left_bottom
  planar_configuration:
    1: chunky
    2: planar
  resolution_unit:
    1: none
    2: inch
    3: centimeter
  threshholding:
    1: no_dithering
    2: ordered_dither
    3: randomized_dither
  fill_order:
    1: msb_to_lsb
    2: lsb_to_msb