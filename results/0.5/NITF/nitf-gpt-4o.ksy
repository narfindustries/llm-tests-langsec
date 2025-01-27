meta:
  id: nitf
  title: National Imagery Transmission Format
  file-extension: nitf
  endian: be

seq:
  - id: header
    type: header

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
  header:
    seq:
      - id: file_type
        type: str
        size: 4
      - id: version
        type: str
        size: 5
      - id: complexity_level
        type: str
        size: 2
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

  image_segment:
    seq:
      - id: segment_header
        type: str
        size: 10
      - id: segment_data
        size: 100  # Placeholder size, replace with actual size logic

  graphic_segment:
    seq:
      - id: segment_header
        type: str
        size: 10
      - id: segment_data
        size: 50  # Placeholder size, replace with actual size logic

  text_segment:
    seq:
      - id: segment_header
        type: str
        size: 10
      - id: segment_data
        size: 200  # Placeholder size, replace with actual size logic

  data_extension_segment:
    seq:
      - id: segment_header
        type: str
        size: 10
      - id: segment_data
        size: 150  # Placeholder size, replace with actual size logic

  reserved_extension_segment:
    seq:
      - id: segment_header
        type: str
        size: 10
      - id: segment_data
        size: 75  # Placeholder size, replace with actual size logic
