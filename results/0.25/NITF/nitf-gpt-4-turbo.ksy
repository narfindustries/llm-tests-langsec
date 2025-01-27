meta:
  id: nitf
  title: National Imagery Transmission Format
  file-extension: nitf
  endian: be
  license: CC0-1.0
  ks-version: 0.9

doc: |
  The NITF (National Imagery Transmission Format) format is a file format developed by the US Government for creating, storing, and transmitting digital imagery and satellite photographs.

seq:
  - id: header
    type: file_header

  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: header.num_image_segments

  - id: graphics_segments
    type: graphics_segment
    repeat: expr
    repeat-expr: header.num_graphics_segments

types:
  file_header:
    seq:
      - id: file_profile_name
        size: 4
        type: str
        encoding: ASCII

      - id: file_version
        size: 5
        type: str
        encoding: ASCII

      - id: complex_file_length
        type: u4

      - id: header_length
        type: u2

      - id: num_image_segments
        type: u2

      - id: num_graphics_segments
        type: u2

  image_segment:
    seq:
      - id: image_id
        size: 10
        type: str
        encoding: ASCII

      - id: image_date_time
        size: 14
        type: str
        encoding: ASCII

      - id: target_id
        size: 17
        type: str
        encoding: ASCII

      - id: image_title
        size: 80
        type: str
        encoding: ASCII

      - id: image_data_length
        type: u4

      - id: image_data
        size: image_data_length

  graphics_segment:
    seq:
      - id: graphic_id
        size: 20
        type: str
        encoding: ASCII

      - id: graphic_name
        size: 20
        type: str
        encoding: ASCII

      - id: graphic_mode
        size: 1
        type: str
        encoding: ASCII

      - id: graphic_data_length
        type: u4

      - id: graphic_data
        size: graphic_data_length