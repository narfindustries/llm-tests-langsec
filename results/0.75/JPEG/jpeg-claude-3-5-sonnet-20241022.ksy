meta:
  id: jpeg
  file-extension: jpg
  endian: be

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: magic
        contents: [0xff]
      - id: marker
        type: u1
      - id: length
        type: u2
        if: marker != 0xff and marker != 0x01 and marker != 0xd0 and marker != 0xd1 and marker != 0xd2 and marker != 0xd3 and marker != 0xd4 and marker != 0xd5 and marker != 0xd6 and marker != 0xd7
      - id: data
        size: length - 2
        if: marker != 0xff and marker != 0x01 and marker != 0xd0 and marker != 0xd1 and marker != 0xd2 and marker != 0xd3 and marker != 0xd4 and marker != 0xd5 and marker != 0xd6 and marker != 0xd7
        type:
          switch-on: marker
          cases:
            0xe0: app0_segment
            0xdb: dqt_segment
            0xc0: sof0_segment
            0xc4: dht_segment
            0xda: sos_segment
            _: raw_segment

  app0_segment:
    seq:
      - id: identifier
        type: str
        size: 5
        encoding: ascii
      - id: version_major
        type: u1
      - id: version_minor
        type: u1
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
        repeat-expr: precision == 0 ? 64 : 128

    instances:
      precision:
        value: (precision_and_id & 0xf0) >> 4
      table_id:
        value: precision_and_id & 0x0f

  sof0_segment:
    seq:
      - id: bits_per_sample
        type: u1
      - id: image_height
        type: u2
      - id: image_width
        type: u2
      - id: num_components
        type: u1
      - id: components
        type: component
        repeat: expr
        repeat-expr: num_components

  component:
    seq:
      - id: component_id
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
        repeat-expr: num_codes_sum

    instances:
      table_class:
        value: (table_info & 0xf0) >> 4
      table_id:
        value: table_info & 0x0f
      num_codes_sum:
        value: num_codes[0] + num_codes[1] + num_codes[2] + num_codes[3] + num_codes[4] + num_codes[5] + num_codes[6] + num_codes[7] + num_codes[8] + num_codes[9] + num_codes[10] + num_codes[11] + num_codes[12] + num_codes[13] + num_codes[14] + num_codes[15]

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
      - id: component_id
        type: u1
      - id: huffman_table_ids
        type: u1

  raw_segment:
    seq:
      - id: raw_data
        size-eos: true