meta:
  id: jpeg
  file-extension: jpg
  endian: le
seq:
  - id: soi
    type: u2
    enum: markers
  - id: segments
    type: segment
    repeat: eos
types:
  segment:
    seq:
      - id: marker
        type: u2
        enum: markers
      - id: length
        type: u2
        if: _root.markers.get(marker) != 'RSTn'
      - id: data
        size: length - 2
        type:
          switch-on: _root.markers.get(marker)
          cases:
            'APP0': app0
            'APP1': app1
            'DQT': dqt
            'DHT': dht
            'SOF0': sof0
            'SOS': sos
            'COM': com
            'DRI': dri
            'SOI': null
            'EOI': null
  app0:
    seq:
      - id: identifier
        type: str
        encoding: ASCII
        size: 5
      - id: version
        type: u2
      - id: units
        type: u1
      - id: x_density
        type: u2
      - id: y_density
        type: u2
      - id: x_thumbnail
        type: u1
      - id: y_thumbnail
        type: u1
      - id: thumbnail_data
        size: x_thumbnail * y_thumbnail * 3
  app1:
    seq:
      - id: identifier
        type: str
        encoding: ASCII
        size: 6
      - id: exif_data
        size-eos: true
  dqt:
    seq:
      - id: tables
        type: quant_table
        repeat: eos
    types:
      quant_table:
        seq:
          - id: precision_and_destination
            type: u1
          - id: table_values
            type: u1
            repeat: expr
            repeat-expr: 64
  dht:
    seq:
      - id: tables
        type: huff_table
        repeat: eos
    types:
      huff_table:
        seq:
          - id: table_class_and_destination
            type: u1
          - id: code_counts
            type: u1
            repeat: expr
            repeat-expr: 16
          - id: code_values
            type: u1
            repeat: sum(code_counts)
  sof0:
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
        type: sof_component
        repeat: expr
        repeat-expr: num_components
    types:
      sof_component:
        seq:
          - id: component_id
            type: u1
          - id: sampling_factors
            type: u1
          - id: quantization_table_destination
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
    types:
      sos_component:
        seq:
          - id: component_id
            type: u1
          - id: dc_table_selector
            type: u1
          - id: ac_table_selector
            type: u1
  com:
    seq:
      - id: comment_text
        type: str
        encoding: ASCII
        size-eos: true
  dri:
    seq:
      - id: restart_interval
        type: u2
enums:
  markers:
    0xFFD8: SOI
    0xFFD9: EOI
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
    0xFFD0: RST0
    0xFFD1: RST1
    0xFFD2: RST2
    0xFFD3: RST3
    0xFFD4: RST4
    0xFFD5: RST5
    0xFFD6: RST6
    0xFFD7: RST7
    0xFFDA: SOS
    0xFFDB: DQT
    0xFFDC: DNL
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
    0xFFF0: JPG0
    0xFFF1: JPG1
    0xFFF2: JPG2
    0xFFF3: JPG3
    0xFFF4: JPG4
    0xFFF5: JPG5
    0xFFF6: JPG6
    0xFFF7: JPG7
    0xFFF8: JPG8
    0xFFF9: JPG9
    0xFFFA: JPG10
    0xFFFB: JPG11
    0xFFFC: JPG12
    0xFFFD: JPG13
    0xFFFE: COM
    0xFFFF: invalid