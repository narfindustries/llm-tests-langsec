meta:
  id: nitf
  title: National Imagery Transmission Format (NITF)
  file-extension: nitf
  license: MIT
  endian: be
seq:
  - id: header
    type: nitf_header
  - id: image_segments
    type: nitf_image_segment
    repeat: eos
types:
  nitf_header:
    seq:
      - id: file_header
        type: nitf_file_header
      - id: image_subheaders
        type: nitf_image_subheader
        repeat: eos
  nitf_file_header:
    seq:
      - id: file_profile_name
        size: 4
      - id: file_version
        size: 5
      - id: complex_level
        size: 2
      - id: standard_type
        size: 4
      - id: originating_station_id
        size: 10
      - id: file_date_and_time
        size: 14
      - id: file_title
        size: 80
      - id: file_security
        type: nitf_security
      - id: file_copy_number
        size: 5
      - id: file_number_of_copies
        size: 5
      - id: encryption
        size: 1
      - id: file_background_color
        size: 3
      - id: originator_name
        size: 24
      - id: originator_phone
        size: 18
  nitf_security:
    seq:
      - id: classification
        size: 1
      - id: codewords
        size: 40
      - id: control_and_handling
        size: 40
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
  nitf_image_subheader:
    seq:
      - id: image_id
        size: 10
      - id: image_date_and_time
        size: 14
      - id: target_id
        size: 17
      - id: image_title
        size: 80
      - id: image_security
        type: nitf_security
      - id: image_compression
        size: 2
      - id: image_representation
        size: 8
      - id: image_category
        size: 8
      - id: image_data
        size-eos: true
  nitf_image_segment:
    seq:
      - id: image_subheader
        type: nitf_image_subheader
      - id: image_data
        size-eos: true