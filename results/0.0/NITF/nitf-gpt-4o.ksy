meta:
  id: nitf
  title: NITF
  file-extension: nitf
  endian: be

seq:
  - id: file_header
    type: file_header

types:
  file_header:
    seq:
      - id: file_type
        size: 4
        contents: 'NITF'
      - id: version
        size: 5
      - id: complexity_level
        size: 2
      - id: st_type
        size: 4
      - id: ostanag_version
        size: 4
      - id: classification
        size: 1
      - id: security_classification_system
        size: 2
      - id: codewords
        size: 11
      - id: control_and_handling
        size: 2
      - id: release_instructions
        size: 20
      - id: declassification_type
        size: 2
      - id: declassification_date
        size: 8
      - id: declassification_exemption
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
      - id: copy_number
        size: 5
      - id: number_of_copies
        size: 5
      - id: encryption
        size: 1
      - id: file_date_time
        size: 14
      - id: file_title
        size: 80
      - id: file_type_id
        size: 35
      - id: file_type_description
        size: 35
      - id: file_structure
        size: 1
      - id: file_header_length
        size: 6
      - id: file_data_length
        size: 10
      - id: number_of_image_segments
        size: 3
      - id: number_of_graphics_segments
        size: 3
      - id: number_of_text_files
        size: 3
      - id: number_of_data_extension_segments
        size: 3
      - id: number_of_reserved_extension_segments
        size: 3
      - id: user_defined_header_data_length
        size: 5
      - id: user_defined_header_overflow
        size: 3
      - id: extended_header_data_length
        size: 5
      - id: extended_header_overflow
        size: 3