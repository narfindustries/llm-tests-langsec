meta:
  id: jpeg
  file-extension: jpg
  endian: be

seq:
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.marker == marker_type::eoi

enums:
  marker_type:
    0xd8: soi
    0xd9: eoi
    0xc0: sof0
    0xc1: sof1
    0xc2: sof2
    0xc3: sof3
    0xc4: dht
    0xdb: dqt
    0xdd: dri
    0xda: sos
    0xe0: app0
    0xe1: app1
    0xe2: app2
    0xe3: app3
    0xe4: app4
    0xe5: app5
    0xe6: app6
    0xe7: app7
    0xe8: app8
    0xe9: app9
    0xea: app10
    0xeb: app11
    0xec: app12
    0xed: app13
    0xee: app14
    0xef: app15
    0xfe: com
  density_units:
    0: none
    1: pixels_per_inch
    2: pixels_per_cm

types:
  segment:
    seq:
      - id: magic
        contents: [0xff]
      - id: marker
        type: u1
        enum: marker_type
      - id: data
        type:
          switch-on: marker
          cases:
            'marker_type::soi': segment_soi
            'marker_type::eoi': segment_eoi
            'marker_type::sof0': segment_sof
            'marker_type::sof1': segment_sof
            'marker_type::sof2': segment_sof
            'marker_type::sof3': segment_sof
            'marker_type::dht': segment_dht
            'marker_type::dqt': segment_dqt
            'marker_type::dri': segment_dri
            'marker_type::sos': segment_sos
            'marker_type::app0': segment_app0
            'marker_type::app1': segment_generic
            'marker_type::app2': segment_generic
            'marker_type::app3': segment_generic
            'marker_type::app4': segment_generic
            'marker_type::app5': segment_generic
            'marker_type::app6': segment_generic
            'marker_type::app7': segment_generic
            'marker_type::app8': segment_generic
            'marker_type::app9': segment_generic
            'marker_type::app10': segment_generic
            'marker_type::app11': segment_generic
            'marker_type::app12': segment_generic
            'marker_type::app13': segment_generic
            'marker_type::app14': segment_generic
            'marker_type::app15': segment_generic
            'marker_type::com': segment_generic

  segment_soi: {}
  segment_eoi: {}

  segment_generic:
    seq:
      - id: length
        type: u2
      - id: data
        size: length - 2

  segment_sof:
    seq:
      - id: length
        type: u2
      - id: sample_precision
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
      - id: component_id
        type: u1
      - id: sampling_factors
        type: u1
      - id: quantization_table_id
        type: u1
    instances:
      sampling_factor_h:
        value: (sampling_factors & 0xf0) >> 4
      sampling_factor_v:
        value: sampling_factors & 0x0f

  segment_dht:
    seq:
      - id: length
        type: u2
      - id: tables
        type: huffman_table
        repeat: eos

  huffman_table:
    seq:
      - id: table_info
        type: u1
      - id: num_codes
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: values
        size: length_of_values
    instances:
      table_class:
        value: (table_info & 0xf0) >> 4
      table_id:
        value: table_info & 0x0f
      length_of_values:
        value: >-
          num_codes[0] + num_codes[1] + num_codes[2] + num_codes[3] +
          num_codes[4] + num_codes[5] + num_codes[6] + num_codes[7] +
          num_codes[8] + num_codes[9] + num_codes[10] + num_codes[11] +
          num_codes[12] + num_codes[13] + num_codes[14] + num_codes[15]

  segment_dqt:
    seq:
      - id: length
        type: u2
      - id: tables
        type: quantization_table
        repeat: eos

  quantization_table:
    seq:
      - id: table_info
        type: u1
      - id: values
        type:
          switch-on: precision
          cases:
            0: u1
            1: u2
        repeat: expr
        repeat-expr: 64
    instances:
      precision:
        value: (table_info & 0xf0) >> 4
      table_id:
        value: table_info & 0x0f

  segment_dri:
    seq:
      - id: length
        type: u2
      - id: restart_interval
        type: u2

  segment_sos:
    seq:
      - id: length
        type: u2
      - id: num_components
        type: u1
      - id: components
        type: scan_component
        repeat: expr
        repeat-expr: num_components
      - id: start_spectral_selection
        type: u1
      - id: end_spectral_selection
        type: u1
      - id: successive_approximation
        type: u1
      - id: image_data
        size-eos: true

  scan_component:
    seq:
      - id: component_id
        type: u1
      - id: huffman_table_ids
        type: u1
    instances:
      dc_table_id:
        value: (huffman_table_ids & 0xf0) >> 4
      ac_table_id:
        value: huffman_table_ids & 0x0f

  segment_app0:
    seq:
      - id: length
        type: u2
      - id: identifier
        type: str
        size: 5
        encoding: ASCII
      - id: version
        type: u2
      - id: units
        type: u1
        enum: density_units
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