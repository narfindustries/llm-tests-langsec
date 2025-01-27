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
  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: header.num_text_segments
  - id: data_extension_segments
    type: data_extension_segment
    repeat: expr
    repeat-expr: header.num_data_extension_segments
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
      - id: num_image_segments
        type: u1
      - id: num_text_segments
        type: u1
      - id: num_data_extension_segments
        type: u1
  image_segment:
    seq:
      - id: header_length
        type: u2
      - id: image_data
        size-eos: true
  text_segment:
    seq:
      - id: header_length
        type: u2
      - id: text_data
        size-eos: true
  data_extension_segment:
    seq:
      - id: header_length
        type: u2
      - id: data
        size-eos: true