meta:
  id: nitf_deepseek_chat
  file-extension: nitf
  endian: be
  imports:
    - nitf_common

seq:
  - id: header
    type: nitf_header
  - id: image_segments
    type: nitf_image_segment
    repeat: expr
    repeat-expr: header.num_image_segments
  - id: graphics_segments
    type: nitf_graphics_segment
    repeat: expr
    repeat-expr: header.num_graphics_segments
  - id: text_segments
    type: nitf_text_segment
    repeat: expr
    repeat-expr: header.num_text_segments
  - id: data_extension_segments
    type: nitf_data_extension_segment
    repeat: expr
    repeat-expr: header.num_data_extension_segments
  - id: reserved_extension_segments
    type: nitf_reserved_extension_segment
    repeat: expr
    repeat-expr: header.num_reserved_extension_segments

types:
  nitf_header:
    seq:
      - id: file_profile_name
        type: str
        size: 4
      - id: file_version
        type: str
        size: 5
      - id: complexity_level
        type: str
        size: 2
      - id: standard_type
        type: str
        size: 4
      - id: originating_station_id
        type: str
        size: 10
      - id: file_date_and_time
        type: str
        size: 14
      - id: file_title
        type: str
        size: 80
      - id: file_security
        type: nitf_security
      - id: file_copy_number
        type: str
        size: 5
      - id: file_number_of_copies
        type: str
        size: 5
      - id: file_classification_system
        type: str
        size: 2
      - id: file_control_number
        type: str
        size: 20
      - id: file_releasing_instructions
        type: str
        size: 11
      - id: file_declassification_type
        type: str
        size: 2
      - id: file_declassification_date
        type: str
        size: 8
      - id: file_declassification_exemption
        type: str
        size: 4
      - id: file_downgrade
        type: str
        size: 1
      - id: file_downgrade_date
        type: str
        size: 8
      - id: file_classification_text
        type: str
        size: 43
      - id: file_classification_authority_type
        type: str
        size: 1
      - id: file_classification_authority
        type: str
        size: 40
      - id: file_classification_reason
        type: str
        size: 1
      - id: file_security_source_date
        type: str
        size: 8
      - id: file_security_control_number
        type: str
        size: 15
      - id: num_image_segments
        type: u2
      - id: num_graphics_segments
        type: u2
      - id: num_text_segments
        type: u2
      - id: num_data_extension_segments
        type: u2
      - id: num_reserved_extension_segments
        type: u2

  nitf_security:
    seq:
      - id: classification
        type: str
        size: 1
      - id: codewords
        type: str
        size: 40
      - id: control_and_handling
        type: str
        size: 40
      - id: release_instructions
        type: str
        size: 40
      - id: declassification_type
        type: str
        size: 2
      - id: declassification_date
        type: str
        size: 8
      - id: declassification_exemption
        type: str
        size: 4
      - id: downgrade
        type: str
        size: 1
      - id: downgrade_date
        type: str
        size: 8
      - id: classification_text
        type: str
        size: 43
      - id: classification_authority_type
        type: str
        size: 1
      - id: classification_authority
        type: str
        size: 40
      - id: classification_reason
        type: str
        size: 1
      - id: security_source_date
        type: str
        size: 8
      - id: security_control_number
        type: str
        size: 15

  nitf_image_segment:
    seq:
      - id: subheader
        type: nitf_image_subheader
      - id: data
        size: subheader.data_length
        type: str

  nitf_image_subheader:
    seq:
      - id: image_id
        type: str
        size: 10
      - id: image_date_and_time
        type: str
        size: 14
      - id: target_id
        type: str
        size: 17
      - id: image_title
        type: str
        size: 80
      - id: image_security
        type: nitf_security
      - id: image_compression
        type: str
        size: 2
      - id: image_compression_rate_code
        type: str
        size: 4
      - id: image_location
        type: str
        size: 21
      - id: image_magnification
        type: str
        size: 4
      - id: image_data_length
        type: u4

  nitf_graphics_segment:
    seq:
      - id: subheader
        type: nitf_graphics_subheader
      - id: data
        size: subheader.data_length
        type: str

  nitf_graphics_subheader:
    seq:
      - id: graphics_id
        type: str
        size: 10
      - id: graphics_date_and_time
        type: str
        size: 14
      - id: graphics_title
        type: str
        size: 80
      - id: graphics_security
        type: nitf_security
      - id: graphics_data_length
        type: u4

  nitf_text_segment:
    seq:
      - id: subheader
        type: nitf_text_subheader
      - id: data
        size: subheader.data_length
        type: str

  nitf_text_subheader:
    seq:
      - id: text_id
        type: str
        size: 10
      - id: text_date_and_time
        type: str
        size: 14
      - id: text_title
        type: str
        size: 80
      - id: text_security
        type: nitf_security
      - id: text_data_length
        type: u4

  nitf_data_extension_segment:
    seq:
      - id: subheader
        type: nitf_data_extension_subheader
      - id: data
        size: subheader.data_length
        type: str

  nitf_data_extension_subheader:
    seq:
      - id: data_extension_id
        type: str
        size: 10
      - id: data_extension_date_and_time
        type: str
        size: 14
      - id: data_extension_title
        type: str
        size: 80
      - id: data_extension_security
        type: nitf_security
      - id: data_extension_data_length
        type: u4

  nitf_reserved_extension_segment:
    seq:
      - id: subheader
        type: nitf_reserved_extension_subheader
      - id: data
        size: subheader.data_length
        type: str

  nitf_reserved_extension_subheader:
    seq:
      - id: reserved_extension_id
        type: str
        size: 10
      - id: reserved_extension_date_and_time
        type: str
        size: 14
      - id: reserved_extension_title
        type: str
        size: 80
      - id: reserved_extension_security
        type: nitf_security
      - id: reserved_extension_data_length
        type: u4