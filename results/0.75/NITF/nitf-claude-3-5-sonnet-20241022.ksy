meta:
  id: nitf
  title: NITF File Format
  file-extension: ntf
  endian: be

seq:
  - id: header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: header.num_image_segments.to_i

types:
  file_header:
    seq:
      - id: file_profile_name
        type: str
        size: 4
        encoding: UTF-8
      - id: file_version
        type: str
        size: 5
        encoding: UTF-8
      - id: complexity_level
        type: str
        size: 2
        encoding: UTF-8
      - id: standard_type
        type: str
        size: 4
        encoding: UTF-8
      - id: originating_station_id
        type: str
        size: 10
        encoding: UTF-8
      - id: date_time
        type: str
        size: 14
        encoding: UTF-8
      - id: file_title
        type: str
        size: 80
        encoding: UTF-8
      - id: file_security_classification
        type: str
        size: 1
        encoding: UTF-8
      - id: num_image_segments
        type: str
        size: 3
        encoding: UTF-8

  image_segment:
    seq:
      - id: subheader
        type: image_subheader
      - id: image_data
        size: subheader.image_length.to_i

  image_subheader:
    seq:
      - id: file_part_type
        type: str
        size: 2
        encoding: UTF-8
      - id: image_id
        type: str
        size: 10
        encoding: UTF-8
      - id: date_time
        type: str
        size: 14
        encoding: UTF-8
      - id: target_id
        type: str
        size: 17
        encoding: UTF-8
      - id: image_title
        type: str
        size: 80
        encoding: UTF-8
      - id: image_security_classification
        type: str
        size: 1
        encoding: UTF-8
      - id: encryption
        type: str
        size: 1
        encoding: UTF-8
      - id: image_source
        type: str
        size: 42
        encoding: UTF-8
      - id: num_significant_rows
        type: str
        size: 8
        encoding: UTF-8
      - id: num_significant_cols
        type: str
        size: 8
        encoding: UTF-8
      - id: pixel_value_type
        type: str
        size: 3
        encoding: UTF-8
      - id: image_representation
        type: str
        size: 8
        encoding: UTF-8
      - id: image_category
        type: str
        size: 8
        encoding: UTF-8
      - id: actual_bits_per_pixel
        type: str
        size: 2
        encoding: UTF-8
      - id: pixel_justification
        type: str
        size: 1
        encoding: UTF-8
      - id: image_coordinate_system
        type: str
        size: 1
        encoding: UTF-8
      - id: image_compression
        type: str
        size: 2
        encoding: UTF-8
      - id: compression_rate_code
        type: str
        size: 4
        encoding: UTF-8
      - id: image_length
        type: str
        size: 10
        encoding: UTF-8