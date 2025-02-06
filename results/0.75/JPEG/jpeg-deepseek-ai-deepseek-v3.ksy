meta:
  id: jpeg
  file-extension: jpg
  endian: be
  license: CC0-1.0
seq:
  - id: soi
    type: soi_marker
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.type == "eoi"
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
        if: marker != 0xFFD8 and marker != 0xFFD9
      - id: data
        size: length - 2
        type:
          switch-on: marker
          cases:
            0xFFC0: frame_header
            0xFFC1: frame_header
            0xFFC2: frame_header
            0xFFC3: frame_header
            0xFFC5: frame_header
            0xFFC6: frame_header
            0xFFC7: frame_header
            0xFFC4: huffman_table
            0xFFDB: quantization_table
            0xFFDA: scan_header
            0xFFDD: restart_interval
            0xFFFE: comment
            0xFFE0: app0
            0xFFE1: app1
  frame_header:
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
  huffman_table:
    seq:
      - id: table_info
        type: u1
      - id: num_codes
        type: u1
        repeat: 16
      - id: symbols
        type: u1
        repeat: eos
  quantization_table:
    seq:
      - id: table_info
        type: u1
      - id: table_data
        type: u1
        repeat: expr
        repeat-expr: 64
  scan_header:
    seq:
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
  scan_component:
    seq:
      - id: component_id
        type: u1
      - id: huffman_tables
        type: u1
  restart_interval:
    seq:
      - id: interval
        type: u2
  comment:
    seq:
      - id: text
        type: str
        encoding: UTF-8
        size-eos: true
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
      - id: thumbnail_width
        type: u1
      - id: thumbnail_height
        type: u1
      - id: thumbnail_data
        type: u1
        repeat: expr
        repeat-expr: thumbnail_width * thumbnail_height * 3
  app1:
    seq:
      - id: identifier
        type: str
        encoding: ASCII
        size: 6
      - id: exif_data
        type: u1
        repeat: eos
  eoi_marker:
    seq:
      - id: marker
        contents: [0xFF, 0xD9]