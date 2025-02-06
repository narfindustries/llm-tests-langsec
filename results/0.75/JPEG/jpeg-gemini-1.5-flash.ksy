meta:
  id: jpeg
  endian: be
seq:
  - id: soi
    type: soi_marker
  - id: segments
    type: seq
    repeat: until_eof
    until_eof: true
  - id: eoi
    type: eoi_marker

types:
  soi_marker:
    seq:
      - id: marker
        type: u2
        enum:
          0xFFD8: soi

  eoi_marker:
    seq:
      - id: marker
        type: u2
        enum:
          0xFFD9: eoi

  segment:
    id: segment
    seq:
      - id: marker
        type: u2
      - id: length
        type: u2
      - id: data
        type: bytes
        size: length - 2

  sof_marker:
    seq:
      - id: marker
        type: u2
        enum:
          0xFFC0: baseline_dct
          0xFFC1: extended_sequential_dct
          0xFFC2: progressive_dct
          0xFFC3: lossless
          0xFFC5: differential_sequential_dct
          0xFFC6: differential_progressive_dct
          0xFFC7: differential_lossless
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
        type: seq
        size: num_components
        items:
          - id: component_id
            type: u1
          - id: h_sampling_factor
            type: u1
          - id: v_sampling_factor
            type: u1
          - id: quantization_table_selector
            type: u1

  dht_marker:
    seq:
      - id: marker
        type: u2
        enum: 0xFFC4: dht
      - id: length
        type: u2
      - id: tables
        type: seq
        repeat: until_end
        until_end: true
        items:
          - id: table_class_and_id
            type: u8
            bits: 5
          - id: counts
            type: u1
            size: 16
          - id: symbols
            type: u1
            size: counts[0..15]

  dqt_marker:
    seq:
      - id: marker
        type: u2
        enum: 0xFFDB: dqt
      - id: length
        type: u2
      - id: tables
        type: seq
        repeat: until_end
        until_end: true
        items:
          - id: table_precision_and_id
            type: u8
            bits: 5
          - id: table_data
            type: u2
            size: 64

  dri_marker:
    seq:
      - id: marker
        type: u2
        enum: 0xFFDD: dri
      - id: length
        type: u2
      - id: restart_interval
        type: u2

  sos_marker:
    seq:
      - id: marker
        type: u2
        enum: 0xFFDA: sos
      - id: length
        type: u2
      - id: num_components
        type: u1
      - id: components
        type: seq
        size: num_components
        items:
          - id: component_id
            type: u1
          - id: dc_huffman_table_selector
            type: u4
            bits: 4
          - id: ac_huffman_table_selector
            type: u4
            bits: 4
      - id: start_spectral_selection
        type: u1
      - id: end_spectral_selection
        type: u1
      - id: successive_approximation
        type: u1


  com_marker:
    seq:
      - id: marker
        type: u2
        enum: 0xFFFE: com
      - id: length
        type: u2
      - id: comment
        type: str
        size: length - 2
        encoding: ASCII

