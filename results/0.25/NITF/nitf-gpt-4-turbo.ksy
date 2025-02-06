meta:
  id: nitf
  title: National Imagery Transmission Format (NITF)
  file-extension: nitf
  endian: be
  license: Unlicense
doc: |
  NITF is a file format created by the US Government for transmission of images and associated metadata.

seq:
  - id: header
    type: file_header

  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: header.num_linfo

  - id: graphics_segments
    type: graphics_segment
    repeat: expr
    repeat-expr: header.num_graphics_segments

  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: header.num_text_files

  - id: data_extension_segments
    type: data_extension_segment
    repeat: expr
    repeat-expr: header.num_data_extension

  - id: reserved_extension_segments
    type: reserved_extension_segment
    repeat: expr
    repeat-expr: header.num_reserved_extension

types:
  file_header:
    seq:
      - id: file_profile_name
        type: str
        size: 4
        encoding: ASCII
      - id: file_version
        type: str
        size: 5
        encoding: ASCII
      - id: complexity_level
        type: str
        size: 2
        encoding: ASCII
      - id: standard_type
        type: str
        size: 4
        encoding: ASCII
      - id: originating_station_id
        type: str
        size: 10
        encoding: ASCII
      - id: file_date_time
        type: str
        size: 14
        encoding: ASCII
      - id: file_title
        type: str
        size: 80
        encoding: ASCII
      - id: file_security
        type: clas_security
      - id: file_copy_number
        type: str
        size: 5
        encoding: ASCII
      - id: file_num_of_copys
        type: str
        size: 5
        encoding: ASCII
      - id: encryption
        type: str
        size: 1
        encoding: ASCII
      - id: file_bg_color
        type: str
        size: 3
        encoding: ASCII
      - id: originator_name
        type: str
        size: 24
        encoding: ASCII
      - id: originator_phone
        type: str
        size: 18
        encoding: ASCII
      - id: file_length
        type: str
        size: 12
        encoding: ASCII
      - id: header_length
        type: str
        size: 6
        encoding: ASCII
      - id: num_linfo
        type: u2
      - id: linfo
        type: length_info
        repeat: expr
        repeat-expr: num_linfo
      - id: num_graphics_segments
        type: u2
      - id: num_text_files
        type: u2
      - id: num_data_extension
        type: u2
      - id: num_reserved_extension
        type: u2

  clas_security:
    seq:
      - id: clas
        type: str
        size: 1
        encoding: ASCII
      - id: security_system
        type: str
        size: 2
        encoding: ASCII
      - id: codewords
        type: str
        size: 11
        encoding: ASCII
      - id: control_and_handling
        type: str
        size: 2
        encoding: ASCII
      - id: releasability
        type: str
        size: 20
        encoding: ASCII
      - id: declass_type
        type: str
        size: 2
        encoding: ASCII
      - id: declass_date
        type: str
        size: 8
        encoding: ASCII
      - id: declass_exemption
        type: str
        size: 4
        encoding: ASCII
      - id: downgrade
        type: str
        size: 1
        encoding: ASCII
      - id: downgrade_date
        type: str
        size: 8
        encoding: ASCII
      - id: classification_text
        type: str
        size: 43
        encoding: ASCII
      - id: classification_authority_type
        type: str
        size: 1
        encoding: ASCII
      - id: classification_authority
        type: str
        size: 40
        encoding: ASCII
      - id: classification_reason
        type: str
        size: 1
        encoding: ASCII
      - id: security_source_date
        type: str
        size: 8
        encoding: ASCII
      - id: security_control_number
        type: str
        size: 15
        encoding: ASCII

  length_info:
    seq:
      - id: length_image_segment
        type: str
        size: 6
        encoding: ASCII
      - id: image_header_length
        type: str
        size: 4
        encoding: ASCII

  image_segment:
    seq:
      - id: header
        type: image_sub_header
      - id: image_data
        size: header.image_data_length

  image_sub_header:
    seq:
      - id: image_id
        type: str
        size: 10
        encoding: ASCII
      - id: image_date_time
        type: str
        size: 14
        encoding: ASCII
      - id: target_id
        type: str
        size: 17
        encoding: ASCII
      - id: image_title
        type: str
        size: 80
        encoding: ASCII
      - id: image_security
        type: clas_security
      - id: encryption
        type: str
        size: 1
        encoding: ASCII
      - id: image_source
        type: str
        size: 42
        encoding: ASCII
      - id: num_sig_rows
        type: u4
      - id: num_sig_cols
        type: u4
      - id: pixel_value_type
        type: str
        size: 3
        encoding: ASCII
      - id: image_representation
        type: str
        size: 8
        encoding: ASCII
      - id: image_category
        type: str
        size: 8
        encoding: ASCII
      - id: actual_bits_per_pixel
        type: str
        size: 2
        encoding: ASCII
      - id: pixel_justification
        type: str
        size: 1
        encoding: ASCII
      - id: image_coordinate_rep
        type: str
        size: 1
        encoding: ASCII
      - id: image_geo_loc
        type: str
        size: 60
        encoding: ASCII
      - id: num_image_bands
        type: u1
      - id: image_bands
        type: image_band
        repeat: expr
        repeat-expr: num_image_bands
      - id: image_data_length
        type: u4

  image_band:
    seq:
      - id: representation
        type: str
        size: 2
        encoding: ASCII
      - id: subrange
        type: str
        size: 6
        encoding: ASCII
      - id: num_luts
        type: u1
      - id: lut_value_length
        type: str
        size: 5
        encoding: ASCII
      - id: luts
        type: lut
        repeat: expr
        repeat-expr: num_luts

  lut:
    seq:
      - id: num_lut_data
        type: u4
      - id: lut_data
        type: u1
        repeat: expr
        repeat-expr: num_lut_data

  graphics_segment:
    seq:
      - id: header
        type: graphics_sub_header
      - id: graphics_data
        size: header.graphics_data_length

  graphics_sub_header:
    seq:
      - id: graphic_id
        type: str
        size: 10
        encoding: ASCII
      - id: graphic_name
        type: str
        size: 20
        encoding: ASCII
      - id: graphic_classification
        type: clas_security
      - id: encryption
        type: str
        size: 1
        encoding: ASCII
      - id: graphic_type
        type: str
        size: 1
        encoding: ASCII
      - id: graphic_display_level
        type: str
        size: 3
        encoding: ASCII
      - id: graphic_attachment_level
        type: str
        size: 3
        encoding: ASCII
      - id: graphic_date
        type: str
        size: 14
        encoding: ASCII
      - id: graphic_title
        type: str
        size: 80
        encoding: ASCII
      - id: graphic_security
        type: clas_security
      - id: graphics_data_length
        type: u4

  text_segment:
    seq:
      - id: header
        type: text_sub_header
      - id: text_data
        size: header.text_data_length

  text_sub_header:
    seq:
      - id: text_id
        type: str
        size: 7
        encoding: ASCII
      - id: text_date_time
        type: str
        size: 14
        encoding: ASCII
      - id: text_title
        type: str
        size: 80
        encoding: ASCII
      - id: text_security
        type: clas_security
      - id: text_format
        type: str
        size: 3
        encoding: ASCII
      - id: text_data_length
        type: u4

  data_extension_segment:
    seq:
      - id: header
        type: data_extension_sub_header
      - id: data
        size: header.data_length

  data_extension_sub_header:
    seq:
      - id: desid
        type: str
        size: 25
        encoding: ASCII
      - id: data_item_overflow
        type: str
        size: 3
        encoding: ASCII
      - id: des_version
        type: str
        size: 2
        encoding: ASCII
      - id: declas
        type: clas_security
      - id: data_length
        type: u4

  reserved_extension_segment:
    seq:
      - id: header
        type: reserved_sub_header
      - id: data
        size: header.data_length

  reserved_sub_header:
    seq:
      - id: res_type_id
        type: str
        size: 25
        encoding: ASCII
      - id: res_version
        type: str
        size: 2
        encoding: ASCII
      - id: res_class
        type: clas_security
      - id: data_length
        type: u4