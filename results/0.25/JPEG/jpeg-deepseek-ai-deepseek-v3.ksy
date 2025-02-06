meta:
  id: jpeg
  file-extension: jpg
  endian: be
seq:
  - id: soi
    type: marker
    enum: markers
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.marker == markers::eoi
types:
  marker:
    seq:
      - id: marker
        type: u2
        enum: markers
  segment:
    seq:
      - id: marker
        type: marker
      - id: length
        type: u2
        if: marker.marker != markers.soi and marker.marker != markers.eoi
      - id: data
        size: length - 2
        type:
          switch-on: marker.marker
          cases:
            markers.sof0: sof0
            markers.sof1: sof1
            markers.sof2: sof2
            markers.sof3: sof3
            markers.dht: dht
            markers.dqt: dqt
            markers.sos: sos
            markers.app0: app0
            markers.app1: app1
            markers.app2: app2
            markers.app3: app3
            markers.app4: app4
            markers.app5: app5
            markers.app6: app6
            markers.app7: app7
            markers.app8: app8
            markers.app9: app9
            markers.app10: app10
            markers.app11: app11
            markers.app12: app12
            markers.app13: app13
            markers.app14: app14
            markers.app15: app15
            markers.com: com
            markers.dri: dri
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
        type: component
        repeat: expr
        repeat-expr: num_components
  sof1:
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
  sof2:
    seq:
      - id: precision
        type: u1
      - id: height
        type: u2
      - id: width
        type: u2
      - id: num_components
        type:极1
      - id: components
        type: component
        repeat: expr
        repeat-expr: num_components
  sof3:
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
      - id: sampling
        type: u1
      - id: quant_table_id
        type: u1
  dht:
    seq:
      - id: tables
        type: huff_table
        repeat: until
        repeat-until: _io.eof
  huff_table:
    seq:
      - id: table_class
        type: u1
      - id: table_id
        type: u1
      - id: code_lengths
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: code_values
        type: u1
        repeat: expr
        repeat-expr: code_lengths.sum
  dqt:
    seq:
      - id: tables
        type: quant_table
        repeat: until
        repeat-until: _io.eof
  quant_table:
    seq:
      - id: precision
        type: u1
     极id: table_id
        type: u1
      - id: values
        type: u1
        repeat: expr
        repeat-expr: 64
  sos:
    seq:
      - id: num_components
        type: u1
      - id: components
        type: sos_component
        repeat: expr
        repeat-expr: num_components
      - id: spectral_selection
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
  app0:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: version
        type: u2
      - id: units
        type: u1
      - id: x_density
        type: u2
      - id: y_density
        type: u2
      - id: thumbnail_width
        type: u1
      - id: thumbnail_height
        type: u1
      - id: thumbnail_data
        type: u1
        size: thumbnail_width * thumbnail_height * 3
  app1:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  app2:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  app3:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  app4:
    seq:
     极id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  app5:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  app6:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  app7:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  app8:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  app9:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  app10:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  app11:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  app12:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  app13:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  app14:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  app15:
    seq:
      - id: identifier
        type: strz
        encoding: ASCII
      - id: data
        type: u1
        size-eos: true
  com:
    seq:
      - id: comment
        type: strz
        encoding: ASCII
  dri:
    seq:
      - id: restart_interval
        type: u2
enums:
  markers:
    0xFFD8: soi
    0xFFD9: eoi
    0xFFC0: sof0
    0xFFC1: sof1
    0xFFC2: sof2
    0xFFC3: sof3
    0xFFC4: dht
    0xFFDB: dqt
    0xFFDA: sos
    0xFFE0: app0
    0xFFE1: app1
    0xFFE2: app2
    0xFFE3: app3
    0xFFE4: app4
    0xFFE5: app5
    0xFFE6: app6
    0xFFE7