meta:
  id: nitf
  file-extension: ntf
  endian: be

seq:
  - id: file_header
    type: file_header_type
  - id: image_segments
    type: image_segment_type
    repeat: expr
    repeat-expr: file_header.num_image_segments

types:
  file_header_type:
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
      - id: num_image_segments
        type: u4be

  image_segment_type:
    seq:
      - id: image_subheader
        type: image_subheader_type
      - id: image_data
        size: image_subheader.image_length

  image_subheader_type:
    seq:
      - id: image_identifier
        type: str
        size: 2
        encoding: ASCII
      - id: image_date_time
        type: str
        size: 14
        encoding: ASCII
      - id: target_identifier
        type: str
        size: 17
        encoding: ASCII
      - id: image_identifier2
        type: str
        size: 10
        encoding: ASCII
      - id: image_security_classification
        type: str
        size: 1
        encoding: ASCII
      - id: encryption
        type: str
        size: 1
        encoding: ASCII
      - id: image_source
        type: str
        size: 42
        encoding: ASCII
      - id: num_significant_rows
        type: u4be
      - id: num_significant_cols
        type: u4be
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
      - id: image_compression
        type: str
        size: 2
        encoding: ASCII
      - id: compression_rate_code
        type: str
        size: 4
        encoding: ASCII
      - id: image_length
        type: u4be