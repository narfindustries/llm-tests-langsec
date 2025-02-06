meta:
  id: jpeg
  file-extension: jpg
  endian: be
  title: JPEG (ISO/IEC 10918)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  JPEG (Joint Photographic Experts Group) image, which is a commonly used method of lossy compression for digital images.

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: marker
        type: u2
      - id: body
        type:
          switch-on: marker
          cases:
            0xffd8: soi
            0xffe0: appn
            0xffe1: appn
            0xffe2: appn
            0xffe3: appn
            0xffe4: appn
            0xffe5: appn
            0xffe6: appn
            0xffe7: appn
            0xffe8: appn
            0xffe9: appn
            0xffea: appn
            0xffeb: appn
            0xffec: appn
            0xffed: appn
            0xffee: appn
            0xffef: appn
            0xffdb: dqt
            0xffc0: sofn
            0xffc1: sofn
            0xffc2: sofn
            0xffc3: sofn
            0xffc4: dht
            0xffda: sos
            0xffd9: eoi
            0xfffe: com
            0xffdd: dri
        size-eos: true

    types:
      soi:
        doc: Start of Image

      appn:
        seq:
          - id: len
            type: u2
          - id: data
            size: len - 2

      dqt:
        seq:
          - id: len
            type: u2
          - id: qt
            type: quantization_table
            repeat: expr
            repeat-expr: (len - 2) / 65

      sofn:
        seq:
          - id: len
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
            type: frame_component
            repeat: expr
            repeat-expr: num_components

      dht:
        seq:
          - id: len
            type: u2
          - id: huffman_tables
            type: huffman_table
            repeat: expr
            repeat-expr: (len - 2) / 17

      sos:
        seq:
          - id: len
            type: u2
          - id: num_components
            type: u1
          - id: components
            type: scan_component
            repeat: expr
            repeat-expr: num_components
          - id: spectral_selection_start
            type: u1
          - id: spectral_selection_end
            type: u1
          - id: successive_approximation
            type: u1

      eoi:
        doc: End of Image

      com:
        seq:
          - id: len
            type: u2
          - id: comment
            type: str
            encoding: ASCII
            size: len - 2

      dri:
        seq:
          - id: len
            type: u2
          - id: restart_interval
            type: u2

  quantization_table:
    seq:
      - id: table_info
        type: u1
      - id: table
        type: u1
        repeat: expr
        repeat-expr: 64

  frame_component:
    seq:
      - id: component_id
        type: u1
      - id: sampling_factors
        type: u1
      - id: quant_table_id
        type: u1

  huffman_table:
    seq:
      - id: table_info
        type: u1
      - id: lengths
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: values
        type: u1
        repeat: expr
        repeat-expr: sum(_.lengths)

  scan_component:
    seq:
      - id: component_id
        type: u1
      - id: huffman_table
        type: u1