meta:
  id: jpeg
  endian: be

seq:
  - id: marker
    type: u2

types:
  soi:
    seq:
      - id: marker
        type: u2
        eq: 65496
      - id: length
        type: u2
      - id: precision
        type: u1
      - id: height
        type: u2
      - id: width
        type: u2
      - id: num_components
        type: u1
      - id: components
        type: component
        repeat: num_components
    enums:
      precision:
        1: 8-bit
        2: 12-bit
        3: 16-bit

  dqt:
    seq:
      - id: marker
        type: u2
        eq: 65499
      - id: length
        type: u2
      - id: precision
        type: u1
      - id: table_id
        type: u1
      - id: table
        type: u1
        repeat: 64
    enums:
      precision:
        0: 8-bit
        1: 16-bit

  dht:
    seq:
      - id: marker
        type: u2
        eq: 65500
      - id: length
        type: u2
      - id: table_class
        type: u1
      - id: table_id
        type: u1
      - id: num_codes
        type: u1
      - id: code_lengths
        type: u1
        repeat: num_codes
      - id: huffman_codes
        type: u1
        repeat: num_codes
    enums:
      table_class:
        0: dc
        1: ac

  dri:
    seq:
      - id: marker
        type: u2
        eq: 65501
      - id: length
        type: u2
      - id: restart_interval
        type: u2

  sos:
    seq:
      - id: marker
        type: u2
        eq: 65498
      - id: length
        type: u2
      - id: num_components
        type: u1
      - id: components
        type: component
        repeat: num_components
      - id: scan_progression
        type: u1
      - id: entropy_coded_data
        type: entropy_coded_data

  eoi:
    seq:
      - id: marker
        type: u2
        eq: 65497
      - id: length
        type: u2

  dhp:
    seq:
      - id: marker
        type: u2
        eq: 57312
      - id: length
        type: u2
      - id: table_id
        type: u1
      - id: table
        type: u1
        repeat: 64

  exp:
    seq:
      - id: marker
        type: u2
        eq: 57313
      - id: length
        type: u2
      - id: component_id
        type: u1
      - id: table_id
        type: u1
      - id: table
        type: u1
        repeat: 64

  component:
    seq:
      - id: component_id
        type: u1
      - id: horizontal_sampling_factor
        type: u1
      - id: vertical_sampling_factor
        type: u1
      - id: quantization_table_id
        type: u1

  entropy_coded_data:
    seq:
      - id: data
        type: u1
        repeat: eos

instances:
  main:
    type: switch_on
    arg: marker
    cases:
      65496: soi
      65497: eoi
      65498: sos
      65499: dqt
      65500: dht
      65501: dri
      57312: dhp
      57313: exp
      57314: dht
      57315: dht
    repeat: eos