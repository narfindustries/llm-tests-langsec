meta:
  id: jpeg
  file-extension: jpg
  endian: be

seq:
  - id: start_of_image
    contents: [0xFF, 0xD8]

  - id: segments
    type: segment
    repeat: until
    repeat-until: _io.is_eof or _root.is_end_of_image

types:
  segment:
    seq:
      - id: marker
        type: u2
      - id: payload_length
        type: u2
        if: marker != 0xFFD9  # EOI marker
      - id: payload
        size: payload_length - 2
        type:
          switch-on: marker
          cases:
            0xFFE0: app0_marker
            0xFFE1: app1_marker
            0xFFDB: dqt_marker
            0xFFC0: sof0_marker
            0xFFC2: sof2_marker
            0xFFC4: dht_marker
            0xFFDD: dri_marker
            0xFFDA: sos_marker
            0xFFD9: end_of_image_marker

  app0_marker:
    seq:
      - id: identifier
        type: str
        size: 5
        encoding: ascii
      - id: version
        type: u2
      - id: density_units
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
        size: thumbnail_width * thumbnail_height * 3
        if: thumbnail_width > 0 and thumbnail_height > 0

  app1_marker:
    seq:
      - id: identifier
        type: str
        size: 5
        encoding: ascii
      - id: payload
        type: str
        size-eos: true
        encoding: utf-8

  dqt_marker:
    seq:
      - id: tables
        type: quantization_table
        repeat: eos

  quantization_table:
    seq:
      - id: precision
        type: b4
      - id: table_id
        type: b4
      - id: table_data
        size: 64 * (precision + 1)

  sof0_marker:
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
      - id: component_id
        type: u1
      - id: sampling_factors
        type: u1
      - id: quantization_table_selector
        type: u1

  sof2_marker:
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

  dht_marker:
    seq:
      - id: tables
        type: huffman_table
        repeat: eos

  huffman_table:
    seq:
      - id: table_class
        type: b4
      - id: table_destination
        type: b4
      - id: code_lengths
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: values
        size-eos: true

  dri_marker:
    seq:
      - id: restart_interval
        type: u2

  sos_marker:
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
      - id: approximation
        type: u1
      - id: compressed_data
        type: str
        size-eos: true
        encoding: utf-8

  sos_component:
    seq:
      - id: component_selector
        type: u1
      - id: dc_ac_huffman_table_selectors
        type: u1

  end_of_image_marker:
    seq:
      - id: marker
        contents: [0xFF, 0xD9]

instances:
  is_end_of_image:
    value: _io.pos >= _io.size