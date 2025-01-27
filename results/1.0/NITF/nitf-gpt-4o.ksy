meta:
  id: nitf
  title: National Imagery Transmission Format
  file-extension: nitf
  endian: be

seq:
  - id: file_header
    type: file_header

  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: file_header.num_image_segments

  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: file_header.num_text_segments

types:
  file_header:
    seq:
      - id: file_type
        size: 4
        contents: "NITF"
      
      - id: version
        size: 5

      - id: complexity_level
        size: 2
        type: u1
      
      - id: system_type
        size: 4
      
      - id: origin_station_id
        size: 10

      - id: num_image_segments
        size: 3
        type: u1

      - id: num_text_segments
        size: 3
        type: u1

  image_segment:
    seq:
      - id: image_id
        size: 10
      
      - id: image_length
        size: 8
        type: u4

      - id: image_data
        size: image_length

  text_segment:
    seq:
      - id: text_id
        size: 7
      
      - id: text_length
        size: 5
        type: u4
      
      - id: text_data
        size: text_length