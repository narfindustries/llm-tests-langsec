meta:
  id: nitf
  title: National Imagery Transmission Format
  file-extension: ntf
  endian: be

seq:
  - id: header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: header.num_image_segments

types:
  file_header:
    seq:
      - id: file_profile_name
        type: str
        encoding: ASCII
        size: 4
      - id: file_version
        type: str
        encoding: ASCII
        size: 5
      - id: complexity_level
        type: str
        encoding: ASCII
        size: 2
      - id: standard_type
        type: str
        encoding: ASCII
        size: 4
      - id: originating_station_id
        type: str
        encoding: ASCII
        size: 10
      - id: date_time
        type: str
        encoding: ASCII
        size: 14
      - id: file_title
        type: str
        encoding: ASCII
        size: 80
      - id: file_security_classification
        type: str
        encoding: ASCII
        size: 1
      - id: encryption
        type: str
        encoding: ASCII
        size: 1
      - id: file_copy_number
        type: str
        encoding: ASCII
        size: 5
      - id: file_num_copies
        type: str
        encoding: ASCII
        size: 5
      - id: num_image_segments
        type: u2

  image_segment:
    seq:
      - id: image_subheader
        type: image_subheader
      - id: image_data
        size: image_subheader.image_length

  image_subheader:
    seq:
      - id: file_part_type
        type: str
        encoding: ASCII
        size: 2
      - id: image_id
        type: str
        encoding: ASCII
        size: 10
      - id: image_date_time
        type: str
        encoding: ASCII
        size: 14
      - id: target_id
        type: str
        encoding: ASCII
        size: 17
      - id: image_security_classification
        type: str
        encoding: ASCII
        size: 1
      - id: encryption
        type: str
        encoding: ASCII
        size: 1
      - id: image_source
        type: str
        encoding: ASCII
        size: 42
      - id: num_significant_rows
        type: u4
      - id: num_significant_cols
        type: u4
      - id: pixel_value_type
        type: str
        encoding: ASCII
        size: 3
      - id: image_representation
        type: str
        encoding: ASCII
        size: 8
      - id: image_category
        type: str
        encoding: ASCII
        size: 8
      - id: actual_bits_per_pixel
        type: u1
      - id: pixel_justification
        type: str
        encoding: ASCII
        size: 1
      - id: image_coordinate_system
        type: str
        encoding: ASCII
        size: 1
      - id: image_compression
        type: str
        encoding: ASCII
        size: 2
      - id: compression_rate_code
        type: str
        encoding: ASCII
        size: 4
      - id: image_length
        type: u4