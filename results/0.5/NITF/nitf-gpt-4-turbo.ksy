meta:
  id: nitf
  title: National Imagery Transmission Format
  file-extension: nitf
  endian: be
  license: CC0-1.0
  ks-version: 0.9

doc: |
  The NITF (National Imagery Transmission Format) format is a file format developed by the U.S. Government for storing imagery and associated metadata. 

seq:
  - id: header
    type: file_header

  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: header.num_image_segments

  - id: graphics_segments
    type: graphic_segment
    repeat: expr
    repeat-expr: header.num_graphics_segments

types:
  file_header:
    seq:
      - id: file_profile_name
        size: 4
      - id: file_version
        size: 5
      - id: complex_level
        type: u1
      - id: system_type
        size: 4
      - id: origin_station_id
        size: 10
      - id: file_date_time
        size: 14
      - id: file_title
        size: 80
      - id: file_security
        type: clasnfo
      - id: num_image_segments
        type: u2
      - id: num_graphics_segments
        type: u2

  clasnfo:
    seq:
      - id: security_class
        size: 1
      - id: security_system
        size: 2
      - id: codewords
        size: 11
      - id: control_and_handling
        size: 2
      - id: releasing_instructions
        size: 20
      - id: declassification_type
        size: 2

  image_segment:
    seq:
      - id: image_subheader
        type: image_subheader
      - id: image_data
        size-eos: true

  image_subheader:
    seq:
      - id: image_id
        size: 10
      - id: image_date_time
        size: 14
      - id: target_id
        size: 17
      - id: image_title
        size: 80
      - id: image_security
        type: clasnfo
      - id: encryped
        type: b1
      - id: image_source
        size: 42
      - id: num_sig_rows
        type: u4
      - id: num_sig_cols
        type: u4
      - id: pixel_value_type
        size: 3
      - id: image_representation
        size: 8
      - id: image_category
        size: 8

  graphic_segment:
    seq:
      - id: graphic_subheader
        type: graphic_subheader
      - id: graphic_data
        size-eos: true

  graphic_subheader:
    seq:
      - id: graphic_id
        size: 20
      - id: graphic_name
        size: 20
      - id: graphic_classification
        type: clasnfo
      - id: encrypted
        type: b1
      - id: graphic_type
        size: 1