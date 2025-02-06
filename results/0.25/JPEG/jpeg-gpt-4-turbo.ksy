meta:
  id: jpeg
  file-extension: jpg
  endian: be
  title: JPEG (ISO/IEC 10918)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  JPEG is a commonly used method of lossy compression for digital images, particularly for those images produced by digital photography. The degree of compression can be adjusted, allowing a selectable tradeoff between storage size and image quality. JPEG typically achieves 10:1 compression with little perceptible loss in image quality.

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: marker
        type: u2
        enum: markers
      - id: length
        type: u2
        if: marker != markers::soi and marker != markers::eoi
      - id: data
        size: length - 2
        type:
          switch-on: marker
          cases:
            markers::dqt: quantization_table
            markers::sof0: start_of_frame
            markers::dht: huffman_table
            markers::sos: start_of_scan
            markers::app0: jfif_header
            markers::app1: exif_header
            _: generic_segment

  markers:
    enums:
      soi: 0xffd8
      eoi: 0xffd9
      dqt: 0xffdb
      sof0: 0xffc0
      dht: 0xffc4
      sos: 0xffda
      app0: 0xffe0
      app1: 0xffe1
      com: 0xfffe

  quantization_table:
    seq:
      - id: table
        type: quantization_table_struct
        repeat: expr
        repeat-expr: (length - 2) / 65

  quantization_table_struct:
    seq:
      - id: precision_and_table_id
        type: u1
      - id: qtable
        type: u1
        repeat: expr
        repeat-expr: 64

  start_of_frame:
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
      - id: quantization_table_id
        type: u1

  huffman_table:
    seq:
      - id: table
        type: huffman_table_struct
        repeat: expr
        repeat-expr: (length - 2) // 17

  huffman_table_struct:
    seq:
      - id: table_class_and_id
        type: u1
      - id: num_codes
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: symbols
        type: u1
        repeat: eos

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

  scan_component:
    seq:
      - id: component_selector
        type: u1
      - id: huffman_table_selector
        type: u1

  jfif_header:
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
      - id: x_thumbnail
        type: u1
      - id: y_thumbnail
        type: u1
      - id: thumbnail
        size: x_thumbnail * y_thumbnail * 3

  exif_header:
    seq:
      - id: identifier
        type: str
        encoding: ASCII
        size: 6
      - id: data
        size: length - 8

  generic_segment:
    seq:
      - id: data
        size: length - 2
        if: length > 2