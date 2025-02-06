seq:
  - id: soi
    type: soi

  - id: markers
    type: seq_of_markers
    repeat: expr
    until: _io.pos >= _io.size

types:
  soi:
    seq:
      - id: marker
        type: uint16_be

  seq_of_markers:
    seq:
      - id: marker
        type: uint16_be
      - id: payload
        type: 
          switch-on: marker
          cases:
            0xd0: 
              type: rst
            0xd1: 
              type: rst
            0xd2: 
              type: rst
            0xd3: 
              type: rst
            0xd4: 
              type: rst
            0xd5: 
              type: rst
            0xd6: 
              type: rst
            0xd7: 
              type: rst
            0xd8: 
              type: null
            0xd9: 
              type: eoi
            0xe0: 
              type: app0
            0xe1: 
              type: app1
            0xc0: 
              type: sof0
            0xc2: 
              type: sof2
            0xc4: 
              type: dht
            0xdb: 
              type: dqt
            0xdc: 
              type: dnl
            0xdd: 
              type: dri
            0xde: 
              type: dhp
            0xdf: 
              type: exp
            else: 
              type: unknown_marker

  app0:
    seq:
      - id: length
        type: uint16_be
      - id: identifier
        type: str(5, encoding: ascii)
      - id: version
        type: uint16_be
      - id: units
        type: uint8
      - id: xdensity
        type: uint16_be
      - id: ydensity
        type: uint16_be
      - id: thumbnail_width
        type: uint8
      - id: thumbnail_height
        type: uint8

  app1:
    seq:
      - id: length
        type: uint16_be
      - id: exif_data
        type: exif

  exif:
    seq:
      - id: header
        type: str(4, encoding: ascii)
      - id: zero
        type: uint16_be
      - id: offset
        type: uint32_be

  sof0:
    seq:
      - id: length
        type: uint16_be
      - id: precision
        type: uint8
      - id: height
        type: uint16_be
      - id: width
        type: uint16_be
      - id: num_components
        type: uint8
      - id: components
        type: seq
        repeat: expr
        until: _io.pos >= (_io.pos + (length - 8))
        seq:
          - id: id
            type: uint8
          - id: sampling_factors
            type: uint8
          - id: quantization_table_id
            type: uint8

  sof2:
    seq:
      - id: length
        type: uint16_be
      - id: precision
        type: uint8
      - id: height
        type: uint16_be
      - id: width
        type: uint16_be
      - id: num_components
        type: uint8
      - id: components
        type: seq
        repeat: expr
        until: _io.pos >= (_io.pos + (length - 8))
        seq:
          - id: id
            type: uint8
          - id: sampling_factors
            type: uint8
          - id: quantization_table_id
            type: uint8

  dht:
    seq:
      - id: length
        type: uint16_be
      - id: ht_info
        type: seq
        repeat: expr
        until: _io.pos >= (_io.pos + (length - 2))
        seq:
          - id: ht
            type: uint8
          - id: num_codes
            type: uint16_be
          - id: values
            type: seq
            repeat: expr
            until: _io.pos >= (_io.pos + (num_codes - 1))
            seq:
              - id: value
                type: uint8

  dqt:
    seq:
      - id: length
        type: uint16_be
      - id: qt_info
        type: seq
        repeat: expr
        until: _io.pos >= (_io.pos + (length - 2))
        seq:
          - id: qt
            type: uint8
          - id: values
            type: seq
            repeat: expr
            until: _io.pos >= (_io.pos + 64)
            seq:
              - id: value
                type: uint8

  dnl:
    seq:
      - id: length
        type: uint16_be
      - id: num_lines
        type: uint16_be

  dri:
    seq:
      - id: length
        type: uint16_be
      - id: restart_interval
        type: uint16_be

  dhp:
    seq:
      - id: length
        type: uint16_be
      - id: hp_info
        type: seq
        repeat: expr
        until: _io.pos >= (_io.pos + (length - 2))
        seq:
          - id: hp
            type: uint8

  exp:
    seq:
      - id: length
        type: uint16_be
      - id: exp_info
        type: seq
        repeat: expr
        until: _io.pos >= (_io.pos + (length - 2))
        seq:
          - id: exp
            type: uint8

  rst:
    seq:
      - id: length
        type: uint16_be

  eoi:
    seq:
      - id: length
        type: uint16_be

  unknown_marker:
    seq:
      - id: length
        type: uint16_be
      - id: data
        type: bytes
        size: length - 2