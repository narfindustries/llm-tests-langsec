meta:
  id: jpeg
  title: JPEG File Format
  file-extension: jpg
  xref:
    mime: image/jpeg
  endian: be

seq:
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.marker == 0xffd9

types:
  segment:
    seq:
      - id: marker
        type: u2
      - id: length
        type: u2
        if: marker != 0xffd8 and marker != 0xffd9
      - id: data
        size: length - 2
        if: marker != 0xffd8 and marker != 0xffd9
        type:
          switch-on: marker
          cases:
            0xffe0: app0_segment
            0xffdb: dqt_segment
            0xffc0: sof0_segment
            0xffc4: dht_segment
            0xffda: sos_segment
            0xffdd: dri_segment
            0xfffe: com_segment

  app0_segment:
    seq:
      - id: identifier
        type: str
        size: 5
        encoding: ASCII
      - id: version_major
        type: u1
      - id: version_minor
        type: u1
      - id: density_units
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

  dqt_segment:
    seq:
      - id: info
        type: u1
      - id: quantization_table
        type: u1
        repeat: expr
        repeat-expr: 64

  sof0_segment:
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

  dht_segment:
    seq:
      - id: info
        type: u1
      - id: num_codes
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: values
        type: u1
        repeat: eos

  sos_segment:
    seq:
      - id: num_components
        type: u1
      - id: components
        type: sos_component
        repeat: expr
        repeat-expr: num_components
      - id: start_spectral
        type: u1
      - id: end_spectral
        type: u1
      - id: approx
        type: u1

  sos_component:
    seq:
      - id: id
        type: u1
      - id: huffman_table
        type: u1

  dri_segment:
    seq:
      - id: restart_interval
        type: u2

  com_segment:
    seq:
      - id: comment
        type: str
        size: _root.segments[_index].length - 2
        encoding: ASCII