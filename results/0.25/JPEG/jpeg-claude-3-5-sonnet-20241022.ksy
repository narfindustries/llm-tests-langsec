meta:
  id: jpeg
  file-extension: jpg
  endian: be

seq:
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.marker == 0xffd9

types:
  segment:
    seq:
      - id: marker_start
        contents: [0xff]
      - id: marker
        type: u1
      - id: length
        type: u2
        if: marker != 0xd8 and marker != 0xd9
      - id: data
        size: length - 2
        if: marker != 0xd8 and marker != 0xd9
        type:
          switch-on: marker
          cases:
            0xe0: app0_segment
            0xdb: dqt_segment
            0xc0: sof0_segment
            0xc4: dht_segment
            0xda: sos_segment

  app0_segment:
    seq:
      - id: magic
        contents: "JFIF"
      - id: version
        type: u2
      - id: density_units
        type: u1
      - id: x_density
        type: u2
      - id: y_density
        type: u2
      - id: thumbnail_width
        type: u1
      - id: thumbnail_height
        type: u1
      - id: thumbnail_data
        size: thumbnail_width * thumbnail_height * 3

  dqt_segment:
    seq:
      - id: tables
        type: dqt_table
        repeat: eos

  dqt_table:
    seq:
      - id: precision_and_id
        type: u1
      - id: values
        type: u1
        repeat: expr
        repeat-expr: 64

  sof0_segment:
    seq:
      - id: bits_per_sample
        type: u1
      - id: height
        type: u2
      - id: width
        type: u2
      - id: num_components
        type: u1
      - id: components
        type: component
        repeat: expr
        repeat-expr: num_components

  component:
    seq:
      - id: id
        type: u1
      - id: sampling_factors
        type: u1
      - id: quantization_table_id
        type: u1

  dht_segment:
    seq:
      - id: tables
        type: huffman_table
        repeat: eos

  huffman_table:
    seq:
      - id: table_info
        type: u1
      - id: num_codes
        type: u1[16]
      - id: values
        type: u1
        repeat: expr
        repeat-expr: num_codes.sum

  sos_segment:
    seq:
      - id: num_components
        type: u1
      - id: components
        type: sos_component
        repeat: expr
        repeat-expr: num_components
      - id: start_spectral
        type: u1
      - id: end_spectral
        type: u1
      - id: approx_bit_pos
        type: u1
      - id: image_data
        size-eos: true

  sos_component:
    seq:
      - id: id
        type: u1
      - id: huffman_table_ids
        type: u1