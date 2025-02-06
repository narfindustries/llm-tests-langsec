meta:
  id: nitf
  file-extension: ntf
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
        size: 9
        encoding: ASCII
      - id: file_version
        type: str
        size: 4
        encoding: ASCII
      - id: complexity_level
        type: str
        size: 2
        encoding: ASCII
      - id: standard_type
        type: str
        size: 1
        encoding: ASCII
      - id: originating_station_id
        type: str
        size: 10
        encoding: ASCII
      - id: file_datetime
        type: str
        size: 14
        encoding: ASCII
      - id: file_title
        type: str
        size: 80
        encoding: ASCII
      - id: security_classification
        type: security_classification
      - id: security_system
        type: str
        size: 2
        encoding: ASCII
      - id: security_code
        type: str
        size: 11
        encoding: ASCII
      - id: security_control_and_release_markings
        type: str
        size: 20
        encoding: ASCII
      - id: security_downgrade
        type: str
        size: 6
        encoding: ASCII
      - id: security_downgrade_datetime
        type: str
        size: 14
        encoding: ASCII
      - id: file_copy_number
        type: str
        size: 5
        encoding: ASCII
      - id: file_number_of_copies
        type: str
        size: 5
        encoding: ASCII
      - id: encryption
        type: str
        size: 1
        encoding: ASCII
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
      - id: user_defined_header_length
        type: u2
      - id: user_defined_header
        type: str
        size: user_defined_header_length
        encoding: ASCII
      - id: extended_header_length
        type: u2
      - id: extended_header
        type: str
        size: extended_header_length
        encoding: ASCII

  security_classification:
    seq:
      - id: classification
        type: str
        size: 1
        encoding: ASCII
      - id: country_code
        type: str
        size: 2
        encoding: ASCII
      - id: release_instructions
        type: str
        size: 20
        encoding: ASCII

  image_segment:
    seq:
      - id: image_subheader
        type: image_subheader
      - id: image_data
        size-eos: true

  image_subheader:
    seq:
      - id: length
        type: u4
      - id: image_identifier
        type: str
        size: 10
        encoding: ASCII
      - id: image_datetime
        type: str
        size: 14
        encoding: ASCII
      - id: image_target_identifier
        type: str
        size: 17
        encoding: ASCII
      - id: image_title
        type: str
        size: 80
        encoding: ASCII
      - id: security_classification
        type: security_classification
      - id: encryption
        type: str
        size: 1
        encoding: ASCII
      - id: image_source
        type: str
        size: 42
        encoding: ASCII
      - id: significant_rows
        type: u4
      - id: significant_columns
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
        size: 3
        encoding: ASCII
      - id: actual_bits_per_pixel
        type: u2
      - id: pixel_justification
        type: str
        size: 1
        encoding: ASCII
      - id: image_coordinate_representation
        type: str
        size: 1
        encoding: ASCII
      - id: image_geolocation
        type: image_geolocation

  image_geolocation:
    seq:
      - id: coords
        type: coordinate
        repeat: expr
        repeat-expr: 4

  coordinate:
    seq:
      - id: latitude
        type: f8
      - id: longitude
        type: f8

  graphic_segment:
    seq:
      - id: graphic_subheader
        type: graphic_subheader
      - id: graphic_data
        size-eos: true

  graphic_subheader:
    seq:
      - id: graphic_identifier
        type: str
        size: 10
        encoding: ASCII
      - id: graphic_name
        type: str
        size: 20
        encoding: ASCII

  text_segment:
    seq:
      - id: text_subheader
        type: text_subheader
      - id: text_data
        size-eos: true

  text_subheader:
    seq:
      - id: text_identifier
        type: str
        size: 10
        encoding: ASCII
      - id: text_datetime
        type: str
        size: 14
        encoding: ASCII
      - id: text_title
        type: str
        size: 80
        encoding: ASCII

  data_extension_segment:
    seq:
      - id: data_extension_subheader
        type: data_extension_subheader
      - id: data_extension_data
        size-eos: true

  data_extension_subheader:
    seq:
      - id: de_identifier
        type: str
        size: 2
        encoding: ASCII

  reserved_extension_segment:
    seq:
      - id: reserved_extension_subheader
        type: reserved_extension_subheader
      - id: reserved_extension_data
        size-eos: true

  reserved_extension_subheader:
    seq:
      - id: re_identifier
        type: str
        size: 2
        encoding: ASCII