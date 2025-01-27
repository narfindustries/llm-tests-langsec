meta:
  id: nitf
  file-extension: nitf
  endian: be
seq:
  - id: header
    type: nitf_header
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
  - id: data_extension_segments
    type: data_extension_segment
    repeat: expr
    repeat-expr: header.num_data_extension_segments
  - id: reserved_extension_segments
    type: reserved_extension_segment
    repeat: expr
    repeat-expr: header.num_reserved_extension_segments

types:
  nitf_header:
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
      - id: originating_station
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
        type: security_info
      - id: num_image_segments
        type: u1
      - id: num_graphic_segments
        type: u1
      - id: num_text_segments
        type: u1
      - id: num_data_extension_segments
        type: u1
      - id: num_reserved_extension_segments
        type: u1
      - id: user_defined_header_length
        type: u2
      - id: extended_header_length
        type: u2

  security_info:
    seq:
      - id: classification
        type: str
        size: 1
        encoding: ASCII
      - id: country_code
        type: str
        size: 2
        encoding: ASCII
      - id: security_group
        type: str
        size: 10
        encoding: ASCII

  image_segment:
    seq:
      - id: header
        type: image_segment_header
      - id: image_data
        size: header.image_data_length

  image_segment_header:
    seq:
      - id: length
        type: u3
      - id: image_data_length
        type: u3

  graphic_segment:
    seq:
      - id: header
        type: graphic_segment_header
      - id: graphic_data
        size: header.graphic_data_length

  graphic_segment_header:
    seq:
      - id: length
        type: u3
      - id: graphic_data_length
        type: u3

  text_segment:
    seq:
      - id: header
        type: text_segment_header
      - id: text_data
        size: header.text_data_length

  text_segment_header:
    seq:
      - id: length
        type: u3
      - id: text_data_length
        type: u3

  data_extension_segment:
    seq:
      - id: header
        type: data_extension_segment_header
      - id: data
        size: header.data_length

  data_extension_segment_header:
    seq:
      - id: length
        type: u3
      - id: data_length
        type: u3

  reserved_extension_segment:
    seq:
      - id: header
        type: reserved_extension_segment_header
      - id: data
        size: header.data_length

  reserved_extension_segment_header:
    seq:
      - id: length
        type: u3
      - id: data_length
        type: u3