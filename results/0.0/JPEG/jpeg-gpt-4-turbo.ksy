meta:
  id: jpeg
  file-extension: jpg
  endian: be
  title: JPEG (ISO/IEC 10918)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  JPEG (Joint Photographic Experts Group) image, a very popular image file format
  that uses lossy compression.

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
            markers::app0: app0
            markers::dqt: dqt
            markers::sof0: sof0
            markers::dht: dht
            markers::sos: sos
            markers::com: com
            _: unknown_data

  unknown_data:
    seq:
      - id: data
        size-eos: true

  app0:
    seq:
      - id: magic
        contents: [0x4A, 0x46, 0x49, 0x46, 0x00] # 'JFIF\0'
      - id: version_major
        type: u1
      - id: version_minor
        type: u1
      - id: density_units
        type: u1
        enum: density_units
      - id: density_x
        type: u2
      - id: density_y
        type: u2
      - id: thumbnail_x
        type: u1
      - id: thumbnail_y
        type: u1
      - id: thumbnail
        size: thumbnail_x * thumbnail_y * 3

  dqt:
    seq:
      - id: tables
        type: quantization_table
        repeat: eos

  quantization_table:
    seq:
      - id: table_info
        type: b1
      - id: table
        type: u1
        repeat: expr
        repeat-expr: 64

  sof0:
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
        type: b1
      - id: quantization_table_id
        type: u1

  dht:
    seq:
      - id: tables
        type: huffman_table
        repeat: eos

  huffman_table:
    seq:
      - id: table_info
        type: b1
      - id: lengths
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: huffman_values
        type: u1
        repeat: eos

  sos:
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
      - id: approx_high
        type: u1
      - id: approx_low
        type: u1

  sos_component:
    seq:
      - id: component_id
        type: u1
      - id: huffman_table_id
        type: b1

  com:
    seq:
      - id: comment
        type: str
        encoding: ASCII
        size-eos: true

enums:
  markers:
    0xffd8: soi
    0xffe0: app0
    0xffdb: dqt
    0xffc0: sof0
    0xffc4: dht
    0xffda: sos
    0xfffe: com
    0xffd9: eoi

  density_units:
    0: no_units
    1: pixels_per_inch
    2: pixels_per_cm