meta:
  id: jpeg
  file-extension: jpg
  endian: be
seq:
  - id: soi
    type: u2
    enum: markers
    contents: [0xFF, 0xD8]
  - id: segments
    type: segment
    repeat: until
    repeat-until: _._io.pos >= _._io.size
types:
  marker:
    seq:
      - id: code
        type: u2
  segment:
    seq:
      - id: marker
        type: u2
        enum: markers
      - id: length
        type: u2
        if: marker != 0xFFD8 and marker != 0xFFD9 and marker != 0xFFDA and marker != 0xFFDD and marker != 0xFFD0 and marker != 0xFFD1 and marker != 0xFFD2 and marker != 0xFFD3 and marker != 0xFFD4 and marker != 0xFFD5 and marker != 0xFFD6 and marker != 0xFFD7
      - id: data
        size: length - 2
        if: marker != 0xFFD8 and marker != 0xFFD9 and marker != 0xFFDA and marker != 0xFFDD and marker != 0xFFD0 and marker != 0xFFD1 and marker != 0xFFD2 and marker != 0xFFD3 and marker != 0xFFD4 and marker != 0xFFD5 and marker != 0xFFD6 and marker != 0xFFD7
  dqt:
    seq:
      - id: precision_and_id
        type: u1
      - id: table
        type: u1
        repeat: expr
        repeat-expr: 64 if (precision_and_id >> 4) == 0 else 128
  dht:
    seq:
      - id: class_and_id
        type: u1
      - id: lengths
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: values
        type: u1
        repeat: expr
        repeat-expr: sum(lengths)
  sof:
    seq:
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
  sos:
    seq:
      - id: num_components
        type: u1
      - id: components
        type: sos_component
        repeat: expr
        repeat-expr: num_components
      - id: spectral_selection_start
        type: u1
      - id: spectral_selection_end
        type: u1
      - id: successive_approximation
        type: u1
  sos_component:
    seq:
      - id: id
        type: u1
      - id: dc_table_id
        type: u1
      - id: ac_table_id
        type: u1
  dri:
    seq:
      - id: restart_interval
        type: u2
  com:
    seq:
      - id: comment
        type: strz
        encoding: ASCII
enums:
  markers:
    0xFFD8: SOI
    0xFFD9: EOI
    0xFFDB: DQT
    0xFFC0: SOF0
    0xFFC1: SOF1
    0xFFC2: SOF2
    0xFFC3: SOF3
    0xFFC4: DHT
    0xFFC5: SOF5
    0xFFC6: SOF6
    0xFFC7: SOF7
    0xFFC8: JPG
    0xFFC9: SOF9
    0xFFCA: SOF10
    0xFFCB: SOF11
    0xFFCC: DAC
    0xFFCD: SOF13
    0xFFCE: SOF14
    0xFFCF: SOF15
    0xFFDA: SOS
    0xFFDD: DRI
    0xFFDE: DHP
    0xFFDF: EXP
    0xFFE0: APP0
    0xFFE1: APP1
    0xFFE2: APP2
    0xFFE3: APP3
    0xFFE4: APP4
    0xFFE5: APP5
    0xFFE6: APP6
    0xFFE7: APP7
    0xFFE8: APP8
    0xFFE9: APP9
    0xFFEA: APP10
    0xFFEB: APP11
    0xFFEC: APP12
    0xFFED: APP13
    0xFFEE: APP14
    0xFFEF: APP15
    0xFFFE: COM
    0xFFD0: RST0
    0xFFD1: RST1
    0xFFD2: RST2
    0xFFD3: RST3
    0xFFD4: RST4
    0xFFD5: RST5
    0xFFD6: RST6
    0xFFD7: RST7