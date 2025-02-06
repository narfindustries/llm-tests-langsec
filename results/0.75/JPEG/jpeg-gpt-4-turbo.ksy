meta:
  id: jpeg
  file-extension: jpg
  endian: be
  title: JPEG (ISO/IEC 10918)

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: marker
        type: u2
      - id: data
        type:
          switch-on: marker
          cases:
            0xffd8: segment_soi
            0xffdb: segment_dqt
            0xffc0: segment_sof0
            0xffc4: segment_dht
            0xffda: segment_sos
            0xffd9: segment_eoi
            0xfffe: segment_com
            0xffe0: segment_app
            0xffe1: segment_app
            0xffe2: segment_app
            0xffe3: segment_app
            0xffe4: segment_app
            0xffe5: segment_app
            0xffe6: segment_app
            0xffe7: segment_app
            0xffe8: segment_app
            0xffe9: segment_app
            0xffea: segment_app
            0xffeb: segment_app
            0xffec: segment_app
            0xffed: segment_app
            0xffee: segment_app
            0xffef: segment_app
            0xffd0: segment_rst
            0xffd1: segment_rst
            0xffd2: segment_rst
            0xffd3: segment_rst
            0xffd4: segment_rst
            0xffd5: segment_rst
            0xffd6: segment_rst
            0xffd7: segment_rst

  segment_soi:
    seq: []

  segment_eoi:
    seq: []

  segment_com:
    seq:
      - id: length
        type: u2
      - id: comment
        type: str
        encoding: ASCII
        size: length - 2

  segment_dqt:
    seq:
      - id: length
        type: u2
      - id: tables
        type: quantization_table
        repeat: until
        repeat-until: _io.pos > _io.size - 2

  quantization_table:
    seq:
      - id: pq_tq
        type: u1
      - id: qk
        type: u1
        repeat: expr
        repeat-expr: 64

  segment_sof0:
    seq:
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

  segment_dht:
    seq:
      - id: length
        type: u2
      - id: tables
        type: huffman_table
        repeat: until
        repeat-until: _io.pos > _io.size - 2

  huffman_table:
    seq:
      - id: tc_th
        type: u1
      - id: li
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: vij
        type: u1
        repeat: expr
        repeat-expr: sum(li)

  segment_sos:
    seq:
      - id: length
        type: u2
      - id: num_components
        type: u1
      - id: components
        type: sos_component
        repeat: expr
        repeat-expr: num_components
      - id: ss
        type: u1
      - id: se
        type: u1
      - id: ah_al
        type: u1

  sos_component:
    seq:
      - id: component_id
        type: u1
      - id: huffman_table_id
        type: u1

  segment_app:
    seq:
      - id: length
        type: u2
      - id: app_data
        type: u1
        repeat: expr
        repeat-expr: length - 2

  segment_rst:
    seq: []