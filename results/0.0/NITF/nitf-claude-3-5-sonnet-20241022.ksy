meta:
  id: nitf
  file-extension: ntf
  endian: be

seq:
  - id: header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: header.num_image_segments
  - id: graphic_segments
    type: graphic_segment
    repeat: expr
    repeat-expr: header.num_graphic_segments
  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: header.num_text_segments

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
      - id: num_image_segments
        type: u2
      - id: num_graphic_segments
        type: u2
      - id: num_text_segments 
        type: u2
      - id: file_length
        type: u4

  image_segment:
    seq:
      - id: subheader
        type: image_subheader
      - id: image_data
        size: subheader.image_length

  image_subheader:
    seq:
      - id: file_part_type
        type: str
        size: 2
        encoding: ASCII
      - id: image_id
        type: str
        size: 10
        encoding: ASCII
      - id: image_date_time
        type: str
        size: 14
        encoding: ASCII
      - id: target_id
        type: str
        size: 17
        encoding: ASCII
      - id: image_title
        type: str
        size: 80
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
      - id: num_rows
        type: u4
      - id: num_cols
        type: u4
      - id: pixel_value_type
        type: str
        size: 3
        encoding: ASCII
      - id: image_compression
        type: str
        size: 2
        encoding: ASCII
      - id: image_band_count
        type: u1
      - id: image_length
        type: u4

  graphic_segment:
    seq:
      - id: subheader
        type: graphic_subheader
      - id: graphic_data
        size: subheader.graphic_length

  graphic_subheader:
    seq:
      - id: file_part_type
        type: str
        size: 2
        encoding: ASCII
      - id: graphic_id
        type: str
        size: 10
        encoding: ASCII
      - id: graphic_name
        type: str
        size: 20
        encoding: ASCII
      - id: graphic_security_classification
        type: str
        size: 1
        encoding: ASCII
      - id: encryption
        type: str
        size: 1
        encoding: ASCII
      - id: graphic_type
        type: str
        size: 1
        encoding: ASCII
      - id: graphic_length
        type: u4

  text_segment:
    seq:
      - id: subheader
        type: text_subheader
      - id: text_data
        size: subheader.text_length

  text_subheader:
    seq:
      - id: file_part_type
        type: str
        size: 2
        encoding: ASCII
      - id: text_id
        type: str
        size: 10
        encoding: ASCII
      - id: text_date_time
        type: str
        size: 14
        encoding: ASCII
      - id: text_title
        type: str
        size: 80
        encoding: ASCII
      - id: text_security_classification
        type: str
        size: 1
        encoding: ASCII
      - id: encryption
        type: str
        size: 1
        encoding: ASCII
      - id: text_format
        type: str
        size: 3
        encoding: ASCII
      - id: text_length
        type: u4