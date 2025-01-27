meta:
  id: nitf
  file-extension: nitf
  endian: le
  license: CC0-1.0
  title: NITF File Format
  ks-version: 0.9

doc: |
  The NITF (National Imagery Transmission Format) standard is used for formatting digital
  imagery and imagery-related products and exchanging them among members of the
  US Intelligence Community (IC) and its partners.

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
        size: 4

      - id: file_version
        type: str
        size: 5

      - id: complexity_level
        type: str
        size: 2

      - id: system_type
        type: str
        size: 4

      - id: origin_station_id
        type: str
        size: 10
        
      - id: file_datetime
        type: str
        size: 14

      - id: file_title
        type: str
        size: 80

      - id: file_security
        type: security_marking_group

      - id: num_image_segments
        type: s2
        encoding: ascii

  security_marking_group:
    seq:
      - id: security_classification
        type: str
        size: 1

      - id: downgrade
        type: str
        size: 6

      - id: classification_authority
        type: str
        size: 40

      - id: classification_reason
        type: str
        size: 1

      - id: security_source_date
        type: str
        size: 8

      - id: security_control_number
        type: str
        size: 15

  image_segment:
    seq:
      - id: image_subheader
        type: image_subheader

      - id: image_data
        size-eos: true

  image_subheader:
    seq:
      - id: image_id_1
        type: str
        size: 10

      - id: image_datetime
        type: str
        size: 14

      - id: target_id
        type: str
        size: 17

      - id: image_title
        type: str
        size: 80

      - id: image_security
        type: security_marking_group

      - id: encryped_index
        type: u1

      - id: image_source
        type: str
        size: 42