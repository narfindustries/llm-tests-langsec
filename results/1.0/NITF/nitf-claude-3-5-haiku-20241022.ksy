meta:
  id: nitf
  title: National Imagery Transmission Format (NITF)
  file-extension: 
    - ntf
    - nitf
  endian: be
  encoding: UTF-8

doc: |
  National Imagery Transmission Format (NITF) specification for geospatial imagery files

seq:
  - id: file_header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: eos

types:
  file_header:
    seq:
      - id: header_id
        contents: 'NITF'
      - id: header_version
        type: str
        size: 2
      - id: file_date_time
        type: str
        size: 14
      - id: file_title
        type: str
        size: 80
      - id: file_length
        type: u4
      - id: header_length
        type: u4
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

  image_segment:
    seq:
      - id: header
        type: image_header
      - id: image_data
        size: header.image_length

  image_header:
    seq:
      - id: header_id
        contents: 'DE'
      - id: image_title
        type: str
        size: 80
      - id: image_length
        type: u4
      - id: image_width
        type: u4  
      - id: image_height
        type: u4
      - id: pixel_type
        type: str
        size: 3
      - id: pixel_bits
        type: u2