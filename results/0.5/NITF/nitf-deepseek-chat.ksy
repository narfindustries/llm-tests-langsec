meta:
  id: nitf
  title: National Imagery Transmission Format (NITF)
  file-extension: nitf
  license: MIT
  endian: be
  encoding: ASCII

seq:
  - id: file_header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: eos
  - id: text_segments
    type: text_segment
    repeat: eos
  - id: data_extension_segments
    type: data_extension_segment
    repeat: eos
  - id: reserved_extension_segments
    type: reserved_extension_segment
    repeat: eos

types:
  file_header:
    seq:
      - id: file_profile_name
        type: str
        size: 4
      - id: file_version
        type: str
        size: 2
      - id: complex_level
        type: u1
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
      - id: classification
        type: str
        size: 1
      - id: file_security
        type: file_security
      - id: file_copy_number
        type: str
        size: 5
      - id: file_number_of_copies
        type: str
        size: 5
      - id: encryption
        type: u1
      - id: file_background_color
        type: rgb_color
      - id: originator_name
        type: str
        size: 24
      - id: originator_phone
        type: str
        size: 18

  file_security:
    seq:
      - id: classification_system
        type: str
        size: 2
      - id: codewords
        type: str
        size: 11
      - id: control_and_handling
        type: str
        size: 2
      - id: releaseability
        type: str
        size: 20
      - id: declass_type
        type: str
        size: 2
      - id: declass_date
        type: str
        size: 8
      - id: declass_exemption
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

  rgb_color:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

  image_segment:
    seq:
      - id: image_subheader
        type: image_subheader
      - id: image_data
        type: image_data

  image_subheader:
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
        type: file_security
      - id: image_compression
        type: str
        size: 2
      - id: image_color
        type: str
        size: 1
      - id: image_representation
        type: str
        size: 8
      - id: image_category
        type: str
        size: 8
      - id: image_data_type
        type: str
        size: 2
      - id: image_resolution_level
        type: u1
      - id: image_location
        type: str
        size: 10
      - id: image_magnification
        type: str
        size: 4
      - id: image_coordinate_system
        type: str
        size: 1
      - id: image_geographic_location
        type: str
        size: 60
      - id: image_number_of_bands
        type: u1
      - id: image_bands
        type: image_band
        repeat: expr
        repeat-expr: image_number_of_bands

  image_band:
    seq:
      - id: band_id
        type: str
        size: 2
      - id: band_representation
        type: str
        size: 2
      - id: band_data_type
        type: str
        size: 2
      - id: band_number_of_blocks
        type: u1
      - id: band_blocks
        type: band_block
        repeat: expr
        repeat-expr: band_number_of_blocks

  band_block:
    seq:
      - id: block_id
        type: str
        size: 2
      - id: block_data
        type: str
        size: 8

  image_data:
    seq:
      - id: data
        type: str
        size-eos: true

  text_segment:
    seq:
      - id: text_subheader
        type: text_subheader
      - id: text_data
        type: str
        size-eos: true

  text_subheader:
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
        type: file_security
      - id: text_format
        type: str
        size: 3
      - id: text_extension
        type: str
        size: 3

  data_extension_segment:
    seq:
      - id: data_extension_subheader
        type: data_extension_subheader
      - id: data_extension_data
        type: str
        size-eos: true

  data_extension_subheader:
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
        type: file_security
      - id: data_extension_format
        type: str
        size: 3
      - id: data_extension_extension
        type: str
        size: 3

  reserved_extension_segment:
    seq:
      - id: reserved_extension_subheader
        type: reserved_extension_subheader
      - id: reserved_extension_data
        type: str
        size-eos: true

  reserved_extension_subheader:
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
        type: file_security
      - id: reserved_extension_format
        type: str
        size: 3
      - id: reserved_extension_extension
        type: str
        size: 3