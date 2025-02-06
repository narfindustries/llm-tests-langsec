seq:
  - id: file_header
    type: file_header
  - id: file_security
    type: file_security
  - id: file_header_extensions
    type: file_header_extension
    repeat: eos
  - id: image_segments
    type: image_segment
    repeat: eos
  - id: graphic_segments
    type: graphic_segment
    repeat: eos
  - id: text_segments
    type: text_segment
    repeat: eos
  - id: file_trailer
    type: file_trailer

types:
  file_header:
    seq:
      - id: file_header_type
        size: 4
      - id: file_format_version
        size: 5
      - id: system_name
        size: 25
      - id: file_name
        size: 24
      - id: file_time
        size: 14
      - id: file_security_classification
        size: 1
        enum: security_classification
      - id: file_title
        size: 80
      - id: file_security_classification_system
        size: 11
      - id: codewords
        size: 15
      - id: control_and_handling
        size: 15
      - id: releaseability
        size: 20
      - id: file_description
        size: 80
      - id: file_extended_section_length
        size: 5

  file_security:
    seq:
      - id: security_classification
        size: 1
        enum: security_classification
      - id: security_classification_system
        size: 11
      - id: codewords
        size: 15
      - id: control_and_handling
        size: 15
      - id: releaseability
        size: 20
      - id: file_description
        size: 80

  file_header_extension:
    seq:
      - id: extension_type
        size: 3
      - id: extension_version
        size: 5
      - id: extension_data
        size: file_header.file_extended_section_length
        process: x => x.trim(0)

  image_segment:
    seq:
      - id: image_header
        type: image_header
      - id: image_data
        type: image_data

  image_header:
    seq:
      - id: image_id
        size: 25
      - id: image_date_and_time
        size: 14
      - id: image_security_classification
        size: 1
        enum: security_classification
      - id: image_representation
        size: 4
      - id: image_compression
        size: 4
      - id: image_data_type
        size: 3
      - id: image_data_format
        size: 4
      - id: image_dimensions
        size: 12
      - id: image_offset
        size: 8

  image_data:
    seq:
      - id: image_data_field
        size: image_header.image_dimensions
        process: x => x.trim(0)

  graphic_segment:
    seq:
      - id: graphic_header
        type: graphic_header
      - id: graphic_data
        type: graphic_data

  graphic_header:
    seq:
      - id: graphic_id
        size: 25
      - id: graphic_date_and_time
        size: 14
      - id: graphic_security_classification
        size: 1
        enum: security_classification
      - id: graphic_type
        size: 6
      - id: graphic_data_type
        size: 3
      - id: graphic_data_format
        size: 4
      - id: graphic_dimensions
        size: 12
      - id: graphic_offset
        size: 8

  graphic_data:
    seq:
      - id: graphic_data_field
        size: graphic_header.graphic_dimensions
        process: x => x.trim(0)

  text_segment:
    seq:
      - id: text_header
        type: text_header
      - id: text_data
        type: text_data

  text_header:
    seq:
      - id: text_id
        size: 25
      - id: text_date_and_time
        size: 14
      - id: text_security_classification
        size: 1
        enum: security_classification
      - id: text_type
        size: 4
      - id: text_data_type
        size: 3
      - id: text_data_format
        size: 4
      - id: text_dimensions
        size: 12
      - id: text_offset
        size: 8

  text_data:
    seq:
      - id: text_data_field
        size: text_header.text_dimensions
        process: x => x.trim(0)

  file_trailer:
    seq:
      - id: file_trailer_type
        size: 4
      - id: file_format_version
        size: 5

enums:
  security_classification:
    - U: 85
    - C: 67
    - S: 83
    - TS: 84