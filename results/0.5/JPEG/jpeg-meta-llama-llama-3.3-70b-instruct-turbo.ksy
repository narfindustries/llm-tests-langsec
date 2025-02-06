meta:
  id: jpeg
  endian: be

seq:
  - id: soi
    type: uint8
  - id: app0
    type: app0
    if: soi == 0xd8
  - id: segments
    type: segment
    repeat: eos
    until: segment.marker == 0xd9

types:
  app0:
    seq:
      - id: marker
        type: uint8
      - id: length
        type: uint16
      - id: identifier
        type: str
        length: 5
      - id: version
        type: uint8
      - id: units
        type: uint8
      - id: x_density
        type: uint16
      - id: y_density
        type: uint16
      - id: thumbnail_width
        type: uint8
      - id: thumbnail_height
        type: uint8

  segment:
    seq:
      - id: marker
        type: uint8
      - id: length
        type: uint16
      - id: body
        type:
          switch-on: marker
          cases:
            '0xc0': sof0
            '0xc2': sof2
            '0xc6': sos
            '0xcc': dqt
            '0xc4': dht
            '0xd0': dri
            '0xd1': dnl
            '0xd2': dhp
            '0xd3': exp
            '0xd4': dht
            '0xd5': dht
            '0xd6': dht
            '0xd7': dht
            '0xc5': dht
            '0xc7': dht
            '0xc8': dht
            '0xc9': dht
            '0xca': dht
            '0xcb': dht
            '0xcd': dht
            '0xce': dht
            '0xcf': dht

  sof0:
    seq:
      - id: precision
        type: uint8
      - id: height
        type: uint16
      - id: width
        type: uint16
      - id: num_components
        type: uint8
      - id: components
        type: component
        repeat: expr
        length: num_components

  sof2:
    seq:
      - id: precision
        type: uint8
      - id: height
        type: uint16
      - id: width
        type: uint16
      - id: num_components
        type: uint8
      - id: components
        type: component
        repeat: expr
        length: num_components

  component:
    seq:
      - id: id
        type: uint8
      - id: sampling_factors
        type: uint8
      - id: quantization_table
        type: uint8

  sos:
    seq:
      - id: num_components
        type: uint8
      - id: components
        type: component_sos
        repeat: expr
        length: num_components
      - id: start_spectral
        type: uint8
      - id: end_spectral
        type: uint8
      - id: successive_approximation
        type: uint8

  component_sos:
    seq:
      - id: id
        type: uint8
      - id: dc_table
        type: uint8
      - id: ac_table
        type: uint8

  dqt:
    seq:
      - id: precision
        type: uint8
      - id: table
        type: uint8
        repeat: expr
        length: 64

  dht:
    seq:
      - id: table_class
        type: uint8
      - id: table_destination
        type: uint8
      - id: num_codes
        type: uint16
      - id: code_lengths
        type: uint8
        repeat: expr
        length: 16
      - id: code_values
        type: uint8
        repeat: expr
        length: num_codes

  dri:
    seq:
      - id: restart_interval
        type: uint16

  dnl:
    seq:
      - id: length
        type: uint16
      - id: num_lines
        type: uint16

  dhp:
    seq:
      - id: length
        type: uint16
      - id: num_lines
        type: uint16

  exp:
    seq:
      - id: length
        type: uint16
      - id: num_lines
        type: uint16