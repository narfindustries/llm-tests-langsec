meta:
  id: nitf
  file-extension: nitf
  endian: be
seq:
  - id: file_header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: file_header.num_image_segments
  - id: graphic_segments
    type: graphic_segment
    repeat: expr
    repeat-expr: file_header.num_graphic_segments
  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: file_header.num_text_segments
  - id: data_extension_segments
    type: data_extension_segment
    repeat: expr
    repeat-expr: file_header.num_data_extension_segments
  - id: reserved_extension_segments
    type: reserved_extension_segment
    repeat: expr
    repeat-expr: file_header.num_reserved_extension_segments
  - id: end_of_file
    type: end_of_file
types:
  file_header:
    seq:
      - id: file_profile_name
        type: str
        encoding: ASCII
        size: 10
      - id: file_version
        type: str
        encoding: ASCII
        size: 2
      - id: file_date_time
        type: str
        encoding: ASCII
        size: 14
      - id: file_title
        type: str
        encoding: ASCII
        size: 80
      - id: security_classification
        type: str
        encoding: ASCII
        size: 1
      - id: codewords
        type: str
        encoding: ASCII
        size: 40
      - id: control_and_handling
        type: str
        encoding: ASCII
        size: 40
      - id: releasing_instructions
        type: str
        encoding: ASCII
        size: 40
      - id: declassification_type
        type: str
        encoding: ASCII
        size: 2
      - id: declassification_date
        type: str
        encoding: ASCII
        size: 8
      - id: declassification_exemption
        type: str
        encoding: ASCII
        size: 4
      - id: downgrade
        type: str
        encoding: ASCII
        size: 1
      - id: downgrade_date
        type: str
        encoding: ASCII
        size: 8
      - id: classification_text
        type: str
        encoding: ASCII
        size: 43
      - id: classification_authority_type
        type: str
        encoding: ASCII
        size: 2
      - id: classification_authority
        type: str
        encoding: ASCII
        size: 40
      - id: classification_reason
        type: str
        encoding: ASCII
        size: 1
      - id: security_source_date
        type: str
        encoding: ASCII
        size: 8
      - id: security_control_number
        type: str
        encoding: ASCII
        size: 15
      - id: num_image_segments
        type: u2
      - id: num_graphic_segments
        type: u2
      - id: num_text_segments
        type: u2
      - id: num_data_extension_segments
        type: u2
      - id: num_reserved_extension_segments
        type: u2
  image_segment:
    seq:
      - id: image_identifier
        type: str
        encoding: ASCII
        size: 10
      - id: image_date_time
        type: str
        encoding: ASCII
        size: 14
      - id: target_identifier
        type: str
        encoding: ASCII
        size: 17
      - id: image_source
        type: str
        encoding: ASCII
        size: 42
      - id: num_rows
        type: u4
      - id: num_columns
        type: u4
      - id: pixel_size
        type: f8
      - id: image_category
        type: str
        encoding: ASCII
        size: 3
      - id: compression_level
        type: u1
  graphic_segment:
    seq:
      - id: graphic_identifier
        type: str
        encoding: ASCII
        size: 10
      - id: graphic_date_time
        type: str
        encoding: ASCII
        size: 14
      - id: graphic_title
        type: str
        encoding: ASCII
        size: 80
      - id: security_classification
        type: str
        encoding: ASCII
        size: 1
      - id: codewords
        type: str
        encoding: ASCII
        size: 40
      - id: control_and_handling
        type: str
        encoding: ASCII
        size: 40
      - id: releasing_instructions
        type: str
        encoding: ASCII
        size: 40
      - id: declassification_type
        type: str
        encoding: ASCII
        size: 2
      - id: declassification_date
        type: str
        encoding: ASCII
        size: 8
      - id: declassification_exemption
        type: str
        encoding: ASCII
        size: 4
      - id: downggrade
        type: str
        encoding: ASCII
        size: 1
      - id: downgrade_date
        type: str
        encoding: ASCII
        size: 8
      - id: classification_text
        type: str
        encoding: ASCII
        size: 43
      - id: classification_authority_type
        type: str
        encoding: ASCII
        size: 2
      - id: classification_authority
        type: str
        encoding: ASCII
        size: 40
      - id: classification_reason
        type: str
        encoding: ASCII
        size: 1
      - id: security_source_date
        type: str
        encoding: ASCII
        size: 8
      - id: security_control_number
        type: str
        encoding: ASCII
        size: 15
      - id: graphic_data
        type: str
        encoding: ASCII
        size-eos: true
  text_segment:
    seq:
      - id: text_identifier
        type: str
        encoding: ASCII
        size: 10
      - id: text_date_time
        type: str
        encoding: ASCII
        size: 14
      - id: text_title
        type: str
        encoding: ASCII
        size: 80
      - id: security_classification
        type: str
        encoding: ASCII
        size: 1
      - id: codewords
        type: str
        encoding: ASCII
        size: 40
      - id: control_and_handling
        type: str
        encoding: ASCII
        size: 40
      - id: releasing_instructions
        type: str
        encoding: ASCII
        size: 40
      - id: declassification_type
        type: str
        encoding: ASCII
        size: 2
      - id: declassification_date
        type: str
        encoding: ASCII
        size: 8
      - id: declassification_exemption
        type: str
        encoding: ASCII
        size: 4
      - id: downggrade
        type: str
        encoding: ASCII
        size: 1
      - id: downgrade_date
        type: str
        encoding: ASCII
        size: 8
      - id: classification_text
        type: str
        encoding: ASCII
        size: 43
      - id: classification_authority_type
        type: str
        encoding: ASCII
        size: 2
      - id: classification_authority
        type: str
        encoding: ASCII
        size: 40
      - id: classification_reason
        type: str
        encoding: ASCII
        size: 1
      - id: security_source_date
        type: str
        encoding: ASCII
        size: 8
      - id: security_control_number
        type: str
        encoding: ASCII
        size: 15
      - id: text_content
        type: str
        encoding: ASCII
        size-eos: true
  data_extension_segment:
    seq:
      - id: data_extension_identifier
        type: str
        encoding: ASCII
        size: 25
      - id: data_extension_version
        type: str
        encoding: ASCII
        size: 2
      - id: data_extension_subheader_length
        type: u2
      - id: data_extension_content
        type: str
        encoding: ASCII
        size-eos: true
  reserved_extension_segment:
    seq:
      - id: reserved_extension_identifier
        type: str
        encoding: ASCII
        size: 25
      - id: reserved_extension_version
        type: str
        encoding: ASCII