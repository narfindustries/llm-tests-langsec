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
    0xda: sos
    0xdd: dri
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
    0xd0: rst0
    0xd1: rst1
    0xd2: rst2
    0xd3: rst3
    0xd4: rst4
    0xd5: rst5
    0xd6: rst6
    0xd7: rst7

types:
  segment:
    seq:
      - id: magic
        contents: [0xff]
      - id: marker
        type: u1
        enum: marker_type
      - id: length
        type: u2
        if: "marker != marker_type::soi and
            marker != marker_type::eoi and
            marker != marker_type::rst0 and
            marker != marker_type::rst1 and
            marker != marker_type::rst2 and
            marker != marker_type::rst3 and
            marker != marker_type::rst4 and
            marker != marker_type::rst5 and
            marker != marker_type::rst6 and
            marker != marker_type::rst7"
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
            'marker_type::sos': start_of_scan
            'marker_type::dri': restart_interval
            'marker_type::app0': app0_segment
            'marker_type::app1': app1_segment
            _: raw_segment
        size: length - 2
        if: "marker != marker_type::soi and
            marker != marker_type::eoi and
            marker != marker_type::rst0 and
            marker != marker_type::rst1 and
            marker != marker_type::rst2 and
            marker != marker_type::rst3 and
            marker != marker_type::rst4 and
            marker != marker_type::rst5 and
            marker != marker_type::rst6 and
            marker != marker_type::rst7"

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
      - id: quantization_table_number
        type: u1

  huffman_table:
    seq:
      - id: tables
        type: huffman_table_spec
        repeat: eos

  huffman_table_spec:
    seq:
      - id: table_class_and_destination
        type: u1
      - id: num_codes_per_length
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: values
        size: num_codes_total
    instances:
      num_codes_total:
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
      - id: precision_and_identifier
        type: u1
      - id: values
        type: u1
        repeat: expr
        repeat-expr: 64
        if: (precision_and_identifier & 0xf0) == 0
      - id: values_16bit
        type: u2
        repeat: expr
        repeat-expr: 64
        if: (precision_and_identifier & 0xf0) != 0

  start_of_scan:
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
      - id: image_data
        size-eos: true

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

  app0_segment:
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
        size: thumbnail_width * thumbnail_height * 3

  app1_segment:
    seq:
      - id: identifier
        type: str
        encoding: ASCII
        size: 6
      - id: exif_data
        size-eos: true

  raw_segment:
    seq:
      - id: data
        size-eos: true