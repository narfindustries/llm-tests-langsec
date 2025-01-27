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
      - id: marker
        type: u2
      - id: length
        type: u2
        if: marker != 0xffd8 and marker != 0xffd9
      - id: data
        size: length - 2
        if: marker != 0xffd8 and marker != 0xffd9
        type:
          switch-on: marker
          cases:
            0xffe0: app0_segment
            0xffe1: app1_segment
            0xffdb: dqt_segment
            0xffc0: sof0_segment
            0xffc2: sof2_segment
            0xffc4: dht_segment
            0xffda: sos_segment

  app0_segment:
    seq:
      - id: magic
        contents: "JFIF\0"
      - id: version_major
        type: u1
      - id: version_minor
        type: u1
      - id: density_units
        type: u1
      - id: density_x
        type: u2
      - id: density_y
        type: u2
      - id: thumbnail_x
        type: u1
      - id: thumbnail_y
        type: u1
      - id: thumbnail_data
        size: thumbnail_x * thumbnail_y * 3

  app1_segment:
    seq:
      - id: exif_magic
        size: 6

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

  sof2_segment:
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
        type: u1
        repeat: expr
        repeat-expr: 16
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

  sos_component:
    seq:
      - id: id
        type: u1
      - id: huffman_table_ids
        type: u1