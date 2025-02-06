meta:
  id: jpeg
  title: JPEG File Format
  license: CC0-1.0
  endian: be
seq:
  - id: soi
    type: soi_marker
  - id: segments
    type: segment
    repeat: until
    repeat-until: _._io.pos >= _._io.size
types:
  soi_marker:
    seq:
      - id: marker
        contents: [0xFF, 0xD8]
  segment:
    seq:
      - id: marker
        type: u2
      - id: length
        type: u2
        if: marker != 0xFFDA and marker != 0xFFD9
      - id: data
        size: length - 2
        type:
          switch-on: marker
          cases:
            0xFFE0: app0
            0xFFDB: dqt
            0xFFC0: sof0
            0xFFC4: dht
            0xFFDA: sos
            0xFFFE: com
  app0:
    seq:
      - id: identifier
        size: 5
        type: str
        encoding: ASCII
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
  dqt:
    seq:
      - id: tables
        type: quantization_table
        repeat: until
        repeat-until: _._io.pos >= _._io.size
  quantization_table:
    seq:
      - id: precision_and_destination
        type: u1
      - id: elements
        size: 64
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
        type: component_spec
        repeat: expr
        repeat-expr: num_components
  component_spec:
    seq:
      - id: component_id
        type: u1
      - id: sampling_factors
        type: u1
      - id: quantization_table_destination
        type: u1
  dht:
    seq:
      - id: tables
        type: huffman_table
        repeat: until
        repeat-until: _._io.pos >= _._io.size
  huffman_table:
    seq:
      - id: class_and_destination
        type: u1
      - id: num_codes
        size: 16
      - id: code_values
        size: _._parent._parent.length - 19
  sos:
    seq:
      - id: num_components
        type: u1
      - id: component_specs
        type: component_spec_sos
        repeat: expr
        repeat-expr: num_components
      - id: spectral_selection_start
        type: u1
      - id: spectral_selection_end
        type: u1
      - id: successive_approximation
        type: u1
      - id: scan_data
        size-eos: true
  component_spec_sos:
    seq:
      - id: component_id
        type: u1
      - id: huffman_table_selectors
        type: u1
  com:
    seq:
      - id: comment
        size-eos: true
        type: str
        encoding: UTF-8