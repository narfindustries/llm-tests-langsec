meta:
  id: nitf
  file-extension: nitf
  endianness: be
  byte-order-mark: 'NITF'
seq:
  - id: header
    type: header
  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: header.num_image_segments
  - id: graphics_segments
    type: graph_segment
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
      - id: file_header
        content: 'NITF'
      - id: file_version
        size: 5
      - id: type
        size: 1
      - id: file_security_classification
        size: 1
      - id: file_control_number
        size: 25
      - id: file_date
        size: 14
      - id: file_title
        size: 80
      - id: file_security_classification_o
        size: 1
      - id: file_downgrade
        size: 1
      - id: file_downgrade_date
        size: 8
      - id: file_downgrade_title
        size: 43
      - id: file_file_name
        size: 24
      - id: file_file_name_o
        size: 5
      - id: num_image_segments
        type: u4
      - id: num_graphic_segments
        type: u4
      - id: num_text_segments
        type: u4
      - id: num_data_extension_segments
        type: u4
      - id: num_reserved_extension_segments
        type: u4
      - id: originator_name
        size: 25
      - id: originator_phone
        size: 20
      - id: file_length_bytes
        type: u6

  image_segment:
    seq:
      - id: image_segment_header
        content: 'IM'
      - id: image_id
        size: 25
      - id: image_date
        size: 14
      - id: image_security_classification
        size: 1
      - id: image_control_number
        size: 20
      - id: image_numericcapturedate
        size: 8
      - id: image_numpix
        type: u4
      - id: image_numlin
        type: u4
      - id: image_lut
        type: image_lut

  graph_segment:
    seq:
      - id: graph_segment_header
        content: 'DE'
      - id: graph_id
        size: 25
      - id: graph_date
        size: 14
      - id: graph_security_classification
        size: 1
      - id: graph_control_number
        size: 20
      - id: graph_numericcapturedate
        size: 8
      - id: graph_symbology_standard
        size: 2

  text_segment:
    seq:
      - id: text_segment_header
        content: 'TE'
      - id: text_id
        size: 25
      - id: text_date
        size: 14
      - id: text_security_classification
        size: 1
      - id: text_control_number
        size: 20
      - id: text_numericcapturedate
        size: 8
      - id: text_text_data
        type: text_data

  data_extension_segment:
    seq:
      - id: data_extension_segment_header
        content: 'DE'
      - id: data_extension_id
        size: 25
      - id: data_extension_date
        size: 14
      - id: data_extension_security_classification
        size: 1
      - id: data_extension_control_number
        size: 20
      - id: data_extension_numericcapturedate
        size: 8
      - id: data_extension_description
        size: 25

  reserved_extension_segment:
    seq:
      - id: reserved_extension_segment_header
        content: 'RE'
      - id: reserved_extension_id
        size: 25
      - id: reserved_extension_date
        size: 14
      - id: reserved_extension_security_classification
        size: 1
      - id: reserved_extension_control_number
        size: 20
      - id: reserved_extension_numericcapturedate
        size: 8
      - id: reserved_extension_description
        size: 25

  image_lut:
    seq:
      - id: lut
        type: u1
        repeat: expr
        repeat-expr: 256