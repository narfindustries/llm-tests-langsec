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
      - id: file_security_classification
        type: str
        size: 1
        encoding: ASCII
      - id: file_codewords
        type: str
        size: 40
        encoding: ASCII
      - id: file_control_and_handling
        type: str
        size: 40
        encoding: ASCII
      - id: file_releasing_instructions
        type: str
        size: 40
        encoding: ASCII
      - id: file_declassification_type
        type: str
        size: 2
        encoding: ASCII
      - id: file_declassification_date
        type: str
        size: 8
        encoding: ASCII
      - id: file_declassification_exemption
        type: str
        size: 4
        encoding: ASCII
      - id: file_downgrade
        type: str
        size: 40
        encoding: ASCII
      - id: file_downgrade_date
        type: str
        size: 8
        encoding: ASCII
      - id: file_classification_text
        type: str
        size: 43
        encoding: ASCII
      - id: file_classification_authority_type
        type: str
        size: 20
        encoding: ASCII
      - id: file_classification_authority
        type: str
        size: 40
        encoding: ASCII
      - id: file_classification_reason
        type: str
        size: 1
        encoding: ASCII
      - id: file_security_source_date
        type: str
        size: 8
        encoding: ASCII
      - id: file_security_control_number
        type: str
        size: 15
        encoding: ASCII
      - id: file_copy_number
        type: str
        size: 5
        encoding: ASCII
      - id: file_number_of_copies
        type: str
        size: 5
        encoding: ASCII
      - id: originator_name
        type: str
        size: 24
        encoding: ASCII
      - id: originator_phone_number
        type: str
        size: 18
        encoding: ASCII
      - id: file_length
        type: u4
      - id: header_length
        type: u2
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
      - id: user_defined_header_data_length
        type: u2
      - id: user_defined_header_overflow
        type: u2
      - id: extended_header_data_length
        type: u2
      - id: extended_header_data_overflow
        type: u2
  image_segment:
    seq:
      - id: image_subheader
        type: image_subheader
      - id: image_data
        size: image_subheader.image_data_length
  image_subheader:
    seq:
      - id: image_identifier_1
        type: str
        size: 10
        encoding: ASCII
      - id: image_date_time
        type: str
        size: 14
        encoding: ASCII
      - id: target_identifier
        type: str
        size: 17
        encoding: ASCII
      - id: image_identifier_2
        type: str
        size: 80
        encoding: ASCII
      - id: image_security_classification
        type: str
        size: 1
        encoding: ASCII
      - id: image_codewords
        type: str
        size: 40
        encoding: ASCII
      - id: image_control_and_handling
        type: str
        size: 40
        encoding: ASCII
      - id: image_releasing_instructions
        type: str
        size: 40
        encoding: ASCII
      - id: image_declassification_type
        type: str
        size: 2
        encoding: ASCII
      - id: image_declassification_date
        type: str
        size: 8
        encoding: ASCII
      - id: image_declassification_exemption
        type: str
        size: 4
        encoding: ASCII
      - id: image_downgrade
        type: str
        size: 40
        encoding: ASCII
      - id: image_downgrade_date
        type: str
        size: 8
        encoding: ASCII
      - id: image_classification_text
        type: str
        size: 43
        encoding: ASCII
      - id: image_classification_authority_type
        type: str
        size: 20
        encoding: ASCII
      - id: image_classification_authority
        type: str
        size: 40
        encoding: ASCII
      - id: image_classification_reason
        type: str
        size: 1
        encoding: ASCII
      - id: image_security_source_date
        type: str
        size: 8
        encoding: ASCII
      - id: image_security_control_number
        type: str
        size: 15
        encoding: ASCII
      - id: image_copy_number
        type: str
        size: 5
        encoding: ASCII
      - id: image_number_of_copies
        type: str
        size: 5
        encoding: ASCII
      - id: encryption
        type: str
        size: 1
        encoding: ASCII
      - id: image_source
        type: str
        size: 42
        encoding: ASCII
      - id: num_rows
        type: u4
      - id: num_columns
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
        type: u1
      - id: pixel_justification
        type: str
        size: 1
        encoding: ASCII
      - id: image_coordinate_system
        type: str
        size: 1
        encoding: ASCII
      - id: image_geographic_location
        type: str
        size: 60
        encoding: ASCII
      - id: num_image_comments
        type: u1
      - id: image_compression
        type: str
        size: 2
        encoding: ASCII
      - id: compression_rate
        type: str
        size: 4
        encoding: ASCII
      - id: num_bands
        type: u1
      - id: extended_num_bands
        type: u1
      - id: image_sync_code
        type: u1
      - id: image_mode
        type: str
        size: 1
        encoding: ASCII
      - id: num_blocks_per_row
        type: u1
      - id: num_blocks_per_column
        type: u1
      - id: num_p