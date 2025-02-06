type: seq
- id: soi
  type: u2
  enum:
    0xFFD8: soi
- id: segments
  type: seq
    - id: type
      type: u1
      read: b
    - id: len
      type: u2
      read: b
    - id: body
      type: bytes
      size: len - 2
  switch: type
  cases:
    0xE0:
      - id: app0
        type: app0
    0xDB:
      - id: dqt
        type: dqt
    0xC4:
      - id: dht
        type: dht
    0xDD:
      - id: dri
        type: dri
    0xDA:
      - id: sos
        type: sos
    0xD9:
      - id: eoi
        type: u2
        enum:
          0xFFD9: eoi
- id: eoi
  type: u2
  enum:
    0xFFD9: eoi

types:
  app0:
    type: struct
    - id: len
      type: u2
    - id: data
      type: bytes
      size: len
  dqt:
    type: struct
    - id: qt_count
      type: u1
      read: b
    - id: qts
      type: seq
      size: qt_count
      - id: precision
        type: u4
        read: b
      - id: id
        type: u4
        read: b
      - id: data
        type: array
        type: u8
        size: 64
  dht:
    type: struct
    - id: count
      type: u1
      read: b
    - id: hts
      type: seq
      size: count
      - id: id
        type: u4
        read: b
      - id: class
        type: u4
        read: b
      - id: data
        type: array
        type: u8
        size: 16
  dri:
    type: struct
    - id: restart_interval
      type: u2
  sos:
    type: struct
    - id: num_components
      type: u1
      read: b
    - id: components
      type: seq
      size: num_components
      - id: id
        type: u1
        read: b
      - id: dc_ht
        type: u1
        read: b
      - id: ac_ht
        type: u1
        read: b
    - id: start_spectral
      type: u1
      read: b
    - id: end_spectral
      type: u1
      read: b
    - id: approx_high
      type: u1
      read: b
    - id: approx_low
      type: u1
      read: b
    - id: data
      type: bytes

