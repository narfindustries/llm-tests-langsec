meta:
  id: tiff
  file-extension: tif
  endian:
    switch-on: byte_order
    cases:
      '"II"': le
      '"MM"': be

seq:
  - id: byte_order
    type: str
    size: 2
    encoding: ascii
  - id: magic_number
    type: u2
    valid: 0x002a
  - id: first_ifd_offset
    type: u4
  - id: ifd
    type: image_file_directory
    pos: first_ifd_offset

types:
  image_file_directory:
    seq:
      - id: num_tags
        type: u2
      - id: tags
        type: tag
        repeat: expr
        repeat-expr: num_tags
      - id: next_ifd_offset
        type: u4

  tag:
    seq:
      - id: tag_id
        type: u2
      - id: data_type
        type: u2
      - id: data_count
        type: u4
      - id: data_value_or_offset
        type: u4
    instances:
      tag_enum:
        value: tag_id
        enum: tag_type
      data_type_enum:
        value: data_type
        enum: data_type_enum

  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4

enums:
  tag_type:
    0x00FE: new_subfile_type
    0x00FF: subfile_type
    0x0100: image_width
    0x0101: image_length
    0x0102: bits_per_sample
    0x0103: compression
    0x0106: photometric_interpretation
    0x0111: strip_offsets
    0x0112: orientation
    0x0115: samples_per_pixel
    0x0116: rows_per_strip
    0x0117: strip_byte_counts
    0x011A: x_resolution
    0x011B: y_resolution
    0x011C: planar_configuration
    0x0128: resolution_unit
    0x0131: software
    0x0132: date_time
    0x013B: artist
    0x013D: predictor
    0x0140: color_map
    0x0152: extra_samples

  data_type_enum:
    1: byte
    2: ascii
    3: short
    4: long
    5: rational

  new_subfile_type_values:
    0: full_resolution_image
    1: reduced_resolution_image
    2: single_page
    3: multi_page_image

  compression_type:
    1: no_compression
    2: ccitt_group3_fax
    3: ccitt_group4_fax
    4: lzw
    5: jpeg
    6: jpeg_alternative
    7: jpeg_full
    32773: packbits

  photometric_interpretation:
    0: white_is_zero
    1: black_is_zero
    2: rgb
    3: palette_color
    4: transparency_mask
    5: cmyk
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
    1: no_unit
    2: inches
    3: centimeters

  predictor:
    1: no_prediction
    2: horizontal_differencing