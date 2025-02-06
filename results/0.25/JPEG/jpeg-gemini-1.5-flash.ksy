type: seq
- id: soi
  type: uint16
  enum:
    0xd8: soi
- id: segments
  type: seq
  - id: segment_type
    type: uint16
  - id: segment_length
    type: uint16
  - id: segment_data
    type: bytes
    size: self.segment_length
    - if: self.segment_type == 0xe0
      id: app0
      type: appn
    - if: self.segment_type == 0xdb
      id: dqt
      type: dqt
    - if: self.segment_type == 0xc4
      id: dht
      type: dht
    - if: self.segment_type == 0xdd
      id: dri
      type: dri
    - if: self.segment_type == 0xda
      id: sos
      type: sos
    - if: self.segment_type == 0xfe
      id: com
      type: com
    - if: self.segment_type == 0xd9
      id: eoi
      type: uint16
      enum:
        0xd9: eoi

types:
  appn:
    seq:
      - id: marker
        type: uint16
      - id: length
        type: uint16
      - id: data
        type: bytes
        size: self.length - 2

  dqt:
    seq:
      - id: marker
        type: uint16
      - id: length
        type: uint16
      - id: tables
        type: seq
        size: (self.length - 2) / 65
        - id: precision
          type: uint8
        - id: table_data
          type: array
          type: uint16
          size: 64

  dht:
    seq:
      - id: marker
        type: uint16
      - id: length
        type: uint16
      - id: tables
        type: seq
        size: (self.length - 2) / 17
        - id: class_id
          type: uint8
        - id: table_id
          type: uint8
        - id: counts
          type: array
          type: uint8
          size: 16
        - id: values
          type: array
          type: uint8
          size: (self.counts.sum)

  dri:
    seq:
      - id: marker
        type: uint16
      - id: length
        type: uint16
      - id: restart_interval
        type: uint16

  sos:
    seq:
      - id: marker
        type: uint16
      - id: length
        type: uint16
      - id: num_components
        type: uint8
      - id: components
        type: seq
        size: self.num_components
        - id: component_id
          type: uint8
        - id: dc_huffman_table_id
          type: uint8
        - id: ac_huffman_table_id
          type: uint8
      - id: start_spectral_selection
        type: uint8
      - id: end_spectral_selection
        type: uint8
      - id: successive_approximation
        type: uint8
      - id: data
        type: bytes

  com:
    seq:
      - id: marker
        type: uint16
      - id: length
        type: uint16
      - id: comment
        type: str
        encoding: UTF-8
        size: self.length - 2
