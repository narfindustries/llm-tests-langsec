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
    0xffd8: soi
    0xffd9: eoi
    0xffc0: sof0
    0xffc1: sof1
    0xffc2: sof2
    0xffc3: sof3
    0xffc4: dht
    0xffdb: dqt
    0xffda: sos
    0xffdd: dri
    0xfffe: com
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
    0xffd0: rst0
    0xffd1: rst1
    0xffd2: rst2
    0xffd3: rst3
    0xffd4: rst4
    0xffd5: rst5
    0xffd6: rst6
    0xffd7: rst7

types:
  segment:
    seq:
      - id: marker
        type: u2
        enum: marker_type
      - id: length
        type: u2
        if: marker != marker_type::soi and marker != marker_type::eoi and
            marker != marker_type::rst0 and marker != marker_type::rst1 and
            marker != marker_type::rst2 and marker != marker_type::rst3 and
            marker != marker_type::rst4 and marker != marker_type::rst5 and
            marker != marker_type::rst6 and marker != marker_type::rst7
      - id: data
        type:
          switch-on: marker
          cases:
            'marker_type::sof0': frame_header
            'marker_type::sof1': frame_header
            'marker_type::sof2': frame_header
            'marker_type::sof3': frame_header
            'marker_type::dht': huffman_table
            'marker_type::dqt': quantization_table
            'marker_type::sos': scan_header
            'marker_type::dri': restart_interval
            'marker_type::com': comment_data
            'marker_type::app0': app_data
            'marker_type::app1': app_data
            'marker_type::app2': app_data
            'marker_type::app3': app_data
            'marker_type::app4': app_data
            'marker_type::app5': app_data
            'marker_type::app6': app_data
            'marker_type::app7': app_data
            'marker_type::app8': app_data
            'marker_type::app9': app_data
            'marker_type::app10': app_data
            'marker_type::app11': app_data
            'marker_type::app12': app_data
            'marker_type::app13': app_data
            'marker_type::app14': app_data
            'marker_type::app15': app_data
        if: marker != marker_type::soi and marker != marker_type::eoi and
            marker != marker_type::rst0 and marker != marker_type::rst1 and
            marker != marker_type::rst2 and marker != marker_type::rst3 and
            marker != marker_type::rst4 and marker != marker_type::rst5 and
            marker != marker_type::rst6 and marker != marker_type::rst7
      - id: image_data
        type: entropy_coded_data
        if: marker == marker_type::sos
        size-eos: true

  frame_header:
    seq:
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
      - id: quantization_table_num
        type: u1

  huffman_table:
    seq:
      - id: tables
        type: huffman_table_spec
        repeat: eos

  huffman_table_spec:
    seq:
      - id: table_class_and_id
        type: u1
      - id: num_codes_per_length
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: values
        size: num_codes_length
    instances:
      num_codes_length:
        value: >-
          num_codes_per_length[0] + num_codes_per_length[1] + 
          num_codes_per_length[2] + num_codes_per_length[3] + 
          num_codes_per_length[4] + num_codes_per_length[5] + 
          num_codes_per_length[6] + num_codes_per_length[7] + 
          num_codes_per_length[8] + num_codes_per_length[9] + 
          num_codes_per_length[10] + num_codes_per_length[11] + 
          num_codes_per_length[12] + num_codes_per_length[13] + 
          num_codes_per_length[14] + num_codes_per_length[15]

  quantization_table:
    seq:
      - id: tables
        type: quantization_table_spec
        repeat: eos

  quantization_table_spec:
    seq:
      - id: precision_and_id
        type: u1
      - id: values
        type: u1
        repeat: expr
        repeat-expr: 64
        if: (precision_and_id & 0xf0) == 0
      - id: values_16bit
        type: u2
        repeat: expr
        repeat-expr: 64
        if: (precision_and_id & 0xf0) != 0

  scan_header:
    seq:
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

  scan_component:
    seq:
      - id: component_id
        type: u1
      - id: dc_ac_table_selector
        type: u1

  restart_interval:
    seq:
      - id: interval
        type: u2

  comment_data:
    seq:
      - id: comment
        type: str
        size: _parent.length - 2
        encoding: ASCII

  app_data:
    seq:
      - id: data
        size: _parent.length - 2

  entropy_coded_data:
    seq:
      - id: data
        size-eos: true