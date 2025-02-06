meta:
  id: jpeg
  title: JPEG (ISO/IEC 10918)
  file-extension: jpg, jpeg
  endian: be
  license: CC0-1.0
seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: marker
        type: u2
    instances:
      body:
        pos: _io.pos
        type:
          switch-on: marker
          cases:
            0xffd8: segment_soi
            0xffc0: segment_sof
            0xffc4: segment_dht
            0xffdb: segment_dqt
            0xffda: segment_sos
            0xffd9: segment_eoi
            0xfffe: segment_com
            '0xffe0': segment_app
            '0xffe1': segment_app
            '0xffe2': segment_app
            '0xffe3': segment_app
            '0xffe4': segment_app
            '0xffe5': segment_app
            '0\xff6': segment_app
            '0xffe7': segment_app
            '0xffe8': segment_app
            '0xffe9': segment_app
            '0xffea': segment_app
            '0xffeb': segment_app
            '0xffec': segment_app
            '0xffed': segment_app
            '0xffee': segment_app
            '0xffef': segment_app
  
  segment_soi:
    # Start of Image
    seq: []

  segment_sof:
    seq:
      - id: precision
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
      - id: id
        type: u1
      - id: sampling_factors
        type: u1
      - id: quantization_table_id
        type: u1

  segment_dht:
    seq:
      - id: huffman_tables
        type: huffman_table
        repeat: eos

  huffman_table:
    seq:
      - id: table_class_and_id
        type: u1
      - id: num_codes
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: symbols
        type: u1
        repeat: eos

  segment_dqt:
    seq:
      - id: quantization_tables
        type: quantization_table
        repeat: eos

  quantization_table:
    seq:
      - id: precision_and_table_id
        type: u1
      - id: table_values
        type: u1
        repeat: expr
        repeat-expr: 64

  segment_sos:
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
      - id: successive_approximation
        type: u1

  sos_component:
    seq:
      - id: component_id
        type: u1
      - id: huffman_table_ids
        type: u1

  segment_eoi:
    # End of Image
    seq: []

  segment_com:
    seq:
      - id: comment
        type: str
        encoding: ASCII
        size-eos: true

  segment_app:
    seq:
      - id: data
        type: u1
        repeat: eos

  segment_unk:
    seq:
      - id: data
        type: u1
        repeat: eos