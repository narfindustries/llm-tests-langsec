meta:
  id: nitf
  endian: be
  title: National Imagery Transmission Format
  file-extension: nitf
  license: CC0-1.0
  ks-version: 0.9

doc: |
  The National Imagery Transmission Format (NITF) is a standard for formatting digital
  imagery and imagery-related products and exchanging them among members of the
  Intelligence Community (IC) as defined by the United States.

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

  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: header.num_text_segments

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

      - id: complexity_level
        type: str
        size: 2
        encoding: ASCII

      - id: system_type
        type: str
        size: 4
        encoding: ASCII

      - id: origin_station_id
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

      - id: file_security
        type: security_marking

      - id: num_image_segments
        type: u2

      - id: num_graphics_segments
        type: u2

      - id: num_text_segments
        type: u2

  security_marking:
    seq:
      - id: classification
        type: str
        size: 1
        encoding: ASCII

      - id: classification_system
        type: str
        size: 2
        encoding: ASCII

      - id: codewords
        type: str
        size: 11
        encoding: ASCII

      - id: control_and_handling
        type: str
        size: 2
        encoding: ASCII

      - id: releaseability
        type: str
        size: 20
        encoding: ASCII

      - id: declass_type
        type: str
        size: 2
        encoding: ASCII

      - id: declass_date
        type: str
        size: 8
        encoding: ASCII

      - id: declass_exemption
        type: str
        size: 4
        encoding: ASCII

      - id: downgrade
        type: str
        size: 1
        encoding: ASCII

      - id: downgrade_date
        type: str
        size: 8
        encoding: ASCII

  image_segment:
    seq:
      - id: image_sub_header
        type: image_sub_header

  image_sub_header:
    seq:
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

  graphics_segment:
    seq:
      - id: graphic_sub_header
        type: graphic_sub_header

  graphic_sub_header:
    seq:
      - id: graphic_id
        type: str
        size: 20
        encoding: ASCII

  text_segment:
    seq:
      - id: text_sub_header
        type: text_sub_header

  text_sub_header:
    seq:
      - id: text_id
        type: str
        size: 7
        encoding: ASCII

      - id: text_date_time
        type: str
        size: 14
        encoding: ASCII