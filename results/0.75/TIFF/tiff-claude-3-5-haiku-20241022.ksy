meta:
  id: tiff
  file-extension: tif
  endian:
    switch-on: byte_order
    cases:
      'little_endian': le
      'big_endian': be

enums:
  byte_order_type:
    0x4949: little_endian
    0x4D4D: big_endian

  subfile_type_enum:
    1: full_resolution
    2: reduced_resolution
    3: multi_page

  compression_type_enum:
    1: no_compression
    2: ccitt_group3_fax
    3: ccitt_group4_fax
    4: lzw
    5: jpeg_old
    6: jpeg
    7: jpeg_new
    32773: packbits

  photometric_interpretation_enum:
    0: white_is_zero
    1: black_is_zero
    2: rgb
    3: rgb_palette
    4: transparency_mask
    5: cmyk
    6: ycbcr

  orientation_enum:
    1: top_left
    2: top_right
    3: bottom_right
    4: bottom_left
    5: left_top
    6: right_top
    7: right_bottom
    8: left_bottom

  planar_configuration_enum:
    1: chunky
    2: planar

  resolution_unit_enum:
    1: no_unit
    2: inch
    3: centimeter

  data_type_enum:
    1: byte
    2: ascii
    3: short
    4: long
    5: rational

seq:
  - id: byte_order
    type: u2
    enum: byte_order_type
  - id: magic_number
    contents: [0x2A, 0x00]
  - id: first_ifd_offset
    type: u4
  - id: image_file_directories
    type: ifd
    repeat: until
    repeat-until: _.next_ifd_offset == 0

types:
  ifd:
    seq:
      - id: tag_count
        type: u2
      - id: tags
        type: tag
        repeat: expr
        repeat-expr: tag_count
      - id: next_ifd_offset
        type: u4
    instances:
      parsed_tags:
        value: tags

  tag:
    seq:
      - id: tag_id
        type: u2
      - id: data_type
        type: u2
        enum: data_type_enum
      - id: data_count
        type: u4
      - id: data_value_or_offset
        type: u4
    instances:
      is_inline_data:
        value: data_count * (_root.type_sizes[data_type.to_s]) <= 4
      data_value:
        value: >-
          is_inline_data ? data_value_or_offset : _io.pos

  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4

  image_data:
    seq:
      - id: strips
        type: strip
        repeat: expr
        repeat-expr: _parent.tag_strip_count

  strip:
    seq:
      - id: data
        size: _parent.tag_strip_byte_count

  exif_ifd:
    seq:
      - id: tag_count
        type: u2
      - id: tags
        type: tag
        repeat: expr
        repeat-expr: tag_count

  gps_ifd:
    seq:
      - id: tag_count
        type: u2
      - id: tags
        type: tag
        repeat: expr
        repeat-expr: tag_count

instances:
  type_sizes:
    value:
      '1': '1'
      '2': '1'
      '3': '2'
      '4': '4'
      '5': '8'

  image_width:
    value: image_file_directories[0].tags[0].data_value_or_offset
    if: image_file_directories[0].tags[0].tag_id == 0x0100
  image_length:
    value: image_file_directories[0].tags[0].data_value_or_offset
    if: image_file_directories[0].tags[0].tag_id == 0x0101
  compression:
    value: image_file_directories[0].tags[0].data_value_or_offset
    enum: compression_type_enum
    if: image_file_directories[0].tags[0].tag_id == 0x0103
  photometric_interpretation:
    value: image_file_directories[0].tags[0].data_value_or_offset
    enum: photometric_interpretation_enum
    if: image_file_directories[0].tags[0].tag_id == 0x0106
  orientation:
    value: image_file_directories[0].tags[0].data_value_or_offset
    enum: orientation_enum
    if: image_file_directories[0].tags[0].tag_id == 0x0112
  samples_per_pixel:
    value: image_file_directories[0].tags[0].data_value_or_offset
    if: image_file_directories[0].tags[0].tag_id == 0x0115
  rows_per_strip:
    value: image_file_directories[0].tags[0].data_value_or_offset
    if: image_file_directories[0].tags[0].tag_id == 0x0116
  strip_byte_counts:
    value: image_file_directories[0].tags[0].data_value_or_offset
    if: image_file_directories[0].tags[0].tag_id == 0x0117
  x_resolution:
    value: image_file_directories[0].tags[0].data_value_or_offset
    if: image_file_directories[0].tags[0].tag_id == 0x011A
  y_resolution:
    value: image_file_directories[0].tags[0].data_value_or_offset
    if: image_file_directories[0].tags[0].tag_id == 0x011B
  planar_configuration:
    value: image_file_directories[0].tags[0].data_value_or_offset
    enum: planar_configuration_enum
    if: image_file_directories[0].tags[0].tag_id == 0x011C
  resolution_unit:
    value: image_file_directories[0].tags[0].data_value_or_offset
    enum: resolution_unit_enum
    if: image_file_directories[0].tags[0].tag_id == 0x0128
  image_description:
    value: image_file_directories[0].tags[0].data_value_or_offset
    if: image_file_directories[0].tags[0].tag_id == 0x010E
  make:
    value: image_file_directories[0].tags[0].data_value_or_offset
    if: image_file_directories[0].tags[0].tag_id == 0x010F
  model:
    value: image_file_directories[0].tags[0].data_value_or_offset
    if: image_file_directories[0].tags[0].tag_id == 0x0110
  software:
    value: image_file_directories[0].tags[0].data_value_or_offset
    if: image_file_directories[0].tags[0].tag_id == 0x0131
  datetime:
    value: image_file_directories[0].tags[0].data_value_or_offset
    if: image_file_directories[0].tags[0].tag_id == 0x0132
  exif_ifd_pointer:
    value: image_file_directories[0].tags[0].data_value_or_offset
    if: image_file_directories[0].tags[0].tag_id == 0x8769
  gps_ifd_pointer:
    value: image_file_directories[0].tags[0].data_value_or_offset
    if: image_file_directories[0].tags[0].tag_id == 0x8825