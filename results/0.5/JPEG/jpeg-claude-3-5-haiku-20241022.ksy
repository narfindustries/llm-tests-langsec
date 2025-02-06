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
    repeat-until: _.marker == 0xFFD9
    
types:
  segment:
    seq:
      - id: marker
        type: u2
      - id: content
        type:
          switch-on: marker
          cases:
            0xFFE0: app0_segment
            0xFFDB: dqt_segment
            0xFFC4: dht_segment
            0xFFC0: sof_segment
            0xFFC2: sof_progressive_segment
            0xFFDA: sos_segment
            0xFFFE: comment_segment
            0xFFD0: restart_marker
            0xFFD1: restart_marker
            0xFFD2: restart_marker
            0xFFD3: restart_marker
            0xFFD4: restart_marker
            0xFFD5: restart_marker
            0xFFD6: restart_marker
            0xFFD7: restart_marker
            0xFFD9: end_of_image_segment
            _: unknown_segment

  app0_segment:
    seq:
      - id: length
        type: u2
      - id: identifier
        type: str
        size: 5
        encoding: ASCII
      - id: version
        type: version
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

  version:
    seq:
      - id: major
        type: u1
      - id: minor
        type: u1

  dqt_segment:
    seq:
      - id: length
        type: u2
      - id: tables
        type: dqt_table
        repeat: eos

  dqt_table:
    seq:
      - id: precision
        type: b4
      - id: table_id
        type: b4
      - id: table_data
        type: u1
        repeat: expr
        repeat-expr: 64

  dht_segment:
    seq:
      - id: length
        type: u2
      - id: tables
        type: dht_table
        repeat: eos

  dht_table:
    seq:
      - id: table_class
        type: b4
      - id: table_destination
        type: b4
      - id: bit_lengths
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: huffman_values
        type: u1
        repeat: eos

  sof_segment:
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
        type: sof_component
        repeat: expr
        repeat-expr: num_components

  sof_progressive_segment:
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
        type: sof_component
        repeat: expr
        repeat-expr: num_components

  sof_component:
    seq:
      - id: component_id
        type: u1
      - id: sampling_factors
        type: u1
      - id: quantization_table_id
        type: u1

  sos_segment:
    seq:
      - id: length
        type: u2
      - id: num_components
        type: u1
      - id: components
        type: sos_component
        repeat: expr
        repeat-expr: num_components
      - id: start_of_spectral_selection
        type: u1
      - id: end_of_spectral_selection
        type: u1
      - id: approximation_bit
        type: u1

  sos_component:
    seq:
      - id: component_id
        type: u1
      - id: dc_ac_table_selectors
        type: u1

  comment_segment:
    seq:
      - id: length
        type: u2
      - id: comment
        type: str
        size: length - 2
        encoding: ASCII

  restart_marker:
    seq:
      - id: marker
        type: u2

  end_of_image_segment:
    seq:
      - id: marker
        contents: [0xFF, 0xD9]

  unknown_segment:
    seq:
      - id: length
        type: u2
      - id: data
        size: length - 2