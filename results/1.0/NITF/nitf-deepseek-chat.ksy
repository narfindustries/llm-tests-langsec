meta:
  id: nitf_deepseek_chat
  title: NITF (National Imagery Transmission Format) DeepSeek Chat
  file-extension: nitf
  endian: be
  license: MIT
  ks-version: 0.9

seq:
  - id: file_header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: eos
  - id: text_segments
    type: text_segment
    repeat: eos
  - id: end_of_file_marker
    size: 4
    contents: "EOF\0"

types:
  file_header:
    seq:
      - id: file_profile_name
        size: 4
        contents: "NITF"
      - id: file_version
        size: 2
        contents: "02.10"
      - id: complexity_level
        type: u1
      - id: standard_type
        size: 2
      - id: originating_station_id
        size: 10
      - id: file_date_and_time
        size: 14
      - id: file_title
        size: 80
      - id: security_classification
        size: 1
      - id: security_system
        size: 2
      - id: codewords
        size: 11
      - id: control_and_handling
        size: 2
      - id: releaseability
        size: 20
      - id: declass_type
        size: 2
      - id: declass_date
        size: 8
      - id: declass_exemption
        size: 4
      - id: downgrade
        size: 1
      - id: downgrade_date
        size: 8
      - id: classification_text
        size: 43
      - id: classification_authority_type
        size: 1
      - id: classification_authority
        size: 40
      - id: classification_reason
        size: 1
      - id: security_source_date
        size: 8
      - id: security_control_number
        size: 15

  image_segment:
    seq:
      - id: image_subheader
        type: image_subheader
      - id: image_data
        size: image_subheader.image_data_length

  image_subheader:
    seq:
      - id: image_subheader_length
        type: u4
      - id: image_id
        size: 10
      - id: image_date_and_time
        size: 14
      - id: image_title
        size: 80
      - id: image_security_classification
        size: 1
      - id: image_security_system
        size: 2
      - id: image_codewords
        size: 11
      - id: image_control_and_handling
        size: 2
      - id: image_releaseability
        size: 20
      - id: image_declass_type
        size: 2
      - id: image_declass_date
        size: 8
      - id: image_declass_exemption
        size: 4
      - id: image_downgrade
        size: 1
      - id: image_downgrade_date
        size: 8
      - id: image_classification_text
        size: 43
      - id: image_classification_authority_type
        size: 1
      - id: image_classification_authority
        size: 40
      - id: image_classification_reason
        size: 1
      - id: image_security_source_date
        size: 8
      - id: image_security_control_number
        size: 15
      - id: image_data_length
        type: u4

  text_segment:
    seq:
      - id: text_subheader
        type: text_subheader
      - id: text_data
        size: text_subheader.text_data_length

  text_subheader:
    seq:
      - id: text_subheader_length
        type: u4
      - id: text_id
        size: 10
      - id: text_date_and_time
        size: 14
      - id: text_title
        size: 80
      - id: text_security_classification
        size: 1
      - id: text_security_system
        size: 2
      - id: text_codewords
        size: 11
      - id: text_control_and_handling
        size: 2
      - id: text_releaseability
        size: 20
      - id: text_declass_type
        size: 2
      - id: text_declass_date
        size: 8
      - id: text_declass_exemption
        size: 4
      - id: text_downgrade
        size: 1
      - id: text_downgrade_date
        size: 8
      - id: text_classification_text
        size: 43
      - id: text_classification_authority_type
        size: 1
      - id: text_classification_authority
        size: 40
      - id: text_classification_reason
        size: 1
      - id: text_security_source_date
        size: 8
      - id: text_security_control_number
        size: 15
      - id: text_data_length
        type: u4