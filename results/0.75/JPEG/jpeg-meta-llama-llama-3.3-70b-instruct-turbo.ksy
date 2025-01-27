meta:
  id: jpeg
  file-extension: jpg
  endian: be

seq:
  - id: soi
    type: u1
    enum: soi_values

  - id: marker
    type: u1
    enum: marker_values

  - id: frames
    type: seq
    repeat: expr
    repeat-expr: marker == marker_values::start_of_frame

  - id: eoi
    type: u1
    enum: eoi_values

types:
  soi_values:
    0xff: soi

  marker_values:
    0xc0: start_of_frame
    0xc4: define_huffman_tables
    0xd9: end_of_image

  eoi_values:
    0xd9: eoi

  frame:
    seq:
      - id: marker
        type: u1
        enum: marker_values

      - id: length
        type: u2

      - id: type
        type: u1
        enum: frame_types

      - id: sampling_factors
        type: u1

      - id: components
        type: seq
        repeat: expr
        repeat-expr: 3

      - id: component
        type: component

    types:
      frame_types:
        0x10: baseline_dct
        0x11: extended_sequential_dct
        0x13: progressive_dct

      component:
        seq:
          - id: id
            type: u1

          - id: sampling_factors
            type: u1

          - id: quantization_table_index
            type: u1