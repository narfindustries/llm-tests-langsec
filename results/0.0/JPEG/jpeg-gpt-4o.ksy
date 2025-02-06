meta:
  id: jpeg
  title: JPEG
  file-extension: jpg
  endian: be

seq:
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.marker == 'EOI'

types:
  segment:
    seq:
      - id: marker
        type: u2
        enum: marker_enum
      - id: length
        type: u2
        if: marker != marker_enum.soi and marker != marker_enum.eoi and marker != marker_enum.com
      - id: data
        size: length - 2
        if: marker != marker_enum.soi and marker != marker_enum.eoi

enums:
  marker_enum:
    0xffd8: soi
    0xffe0: app0
    0xffe1: app1
    0xffe2: app2
    0xffe3: app3
    0xffe4: app4
    0xffe5: app5
    0xffe6: app6
    0xffe7: app7
    0xffe8: app8
    0xffe9: app9
    0xffea: app10
    0xffeb: app11
    0xffec: app12
    0xffed: app13
    0xffee: app14
    0xffef: app15
    0xffdb: dqt
    0xffc0: sof0
    0xffc1: sof1
    0xffc2: sof2
    0xffc3: sof3
    0xffc5: sof5
    0xffc6: sof6
    0xffc7: sof7
    0xffc9: sof9
    0xffca: sof10
    0xffcb: sof11
    0xffcd: sof13
    0xffce: sof14
    0xffcf: sof15
    0xffc4: dht
    0xffda: sos
    0xffd9: eoi
    0xfffe: com

types:
  app0:
    seq:
      - id: identifier
        type: str
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
      - id: thumbnail
        size: x_thumbnail * y_thumbnail * 3

  dqt:
    seq:
      - id: info
        type: u1
      - id: quantization_table
        type: u1
        repeat: expr
        repeat-expr: 64

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

  dht:
    seq:
      - id: info
        type: u1
      - id: num_symbols
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: symbols
        type: u1
        repeat: eos

  sos:
    seq:
      - id: num_components
        type: u1
      - id: components
        type: sos_component
        repeat: expr
        repeat-expr: num_components
      - id: start_spectral_selection
        type: u1
      - id: end_spectral_selection
        type: u1
      - id: successive_approximation
        type: u1

  sos_component:
    seq:
      - id: id
        type: u1
      - id: huffman_table
        type: u1

instances:
  app0_data:
    pos: segments._io.pos
    size: segments.length - 2
    type: app0
    if: segments.marker == marker_enum.app0

  dqt_data:
    pos: segments._io.pos
    size: segments.length - 2
    type: dqt
    if: segments.marker == marker_enum.dqt

  sof_data:
    pos: segments._io.pos
    size: segments.length - 2
    type: sof
    if: segments.marker in [marker_enum.sof0, marker_enum.sof1, marker_enum.sof2, marker_enum.sof3, marker_enum.sof5, marker_enum.sof6, marker_enum.sof7, marker_enum.sof9, marker_enum.sof10, marker_enum.sof11, marker_enum.sof13, marker_enum.sof14, marker_enum.sof15]

  dht_data:
    pos: segments._io.pos
    size: segments.length - 2
    type: dht
    if: segments.marker == marker_enum.dht

  sos_data:
    pos: segments._io.pos
    size: segments.length - 2
    type: sos
    if: segments.marker == marker_enum.sos

  com_data:
    pos: segments._io.pos
    size: segments.length - 2
    type: str
    encoding: utf-8
    if: segments.marker == marker_enum.com