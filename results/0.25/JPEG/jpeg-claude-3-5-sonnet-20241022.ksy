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
    0xfe: com
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
            _: raw_data

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
      - id: quantization_table_id
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
      - id: code_lengths
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: values
        type: u1
        repeat: expr
        repeat-expr: code_lengths[0] + code_lengths[1] + code_lengths[2] + code_lengths[3] + 
                    code_lengths[4] + code_lengths[5] + code_lengths[6] + code_lengths[7] + 
                    code_lengths[8] + code_lengths[9] + code_lengths[10] + code_lengths[11] + 
                    code_lengths[12] + code_lengths[13] + code_lengths[14] + code_lengths[15]

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

  start_of_scan:
    seq:
      - id: num_components
        type: u1
      - id: components
        type: scan_component
        repeat: expr
        repeat-expr: num_components
      - id: start_spectral
        type: u1
      - id: end_spectral
        type: u1
      - id: approx_high_low
        type: u1
      - id: image_data
        size-eos: true

  scan_component:
    seq:
      - id: component_id
        type: u1
      - id: dc_ac_table_ids
        type: u1

  restart_interval:
    seq:
      - id: interval
        type: u2

  app0_segment:
    seq:
      - id: identifier
        type: str
        size: 5
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
        size: thumbnail_width * thumbnail_height * 3
        if: thumbnail_width != 0 and thumbnail_height != 0

  raw_data:
    seq:
      - id: data
        size-eos: true