meta:
  id: jpeg
  file-extension: jpg
  title: JPEG
  endianness: le
seq:
  - id: soi
    type: u1
  - id: block_length
    type: u2
  - id: APP0
    type: /APP0
  - id: blocks
    type: block
    repeat: until-eof
types:
  APP0:
    seq:
      - id: marker
        type: u1
      - id: identifier
        type: str(5, 'ASCII')
      - id: version
        type: u1
      - id: units
        type: u1
      - id: X_density
        type: u2
      - id: Y_density
        type: u2
      - id: thumbnail_width
        type: u1
      - id: thumbnail_height
        type: u1
  block:
    seq:
      - id: marker
        type: u1
      - id: block_type
        type: u1
      - id: block_data
        type:
          switch-on: block_type
          cases:
            '\xFF\xC0': restart
            '\xFF\xC4': define_huffman_tables
            '\xFF\xDA': start_of_scan
          else: raw
  restart:
    seq:
      - id: length
        type: u2
      - id: restart_interval
        type: u2
      - id: components
        type: u1
  define_huffman_tables:
    seq:
      - id: length
        type: u2
      - id: tables
        type: huffman_table
        repeat: expr
          value: length - 2
  start_of_scan:
    seq:
      - id: length
        type: u2
      - id: components
        type: u1
      - id: component_parameters
        type: component_parameter
        repeat: expr
          value: components
      - id: spectral_start
        type: u1
      - id: spectral_end
        type: u1
      - id: successive_approximation
        type: u1
      - id: image_data
        type: u1
        repeat: until
          - marker
  huffman_table:
    seq:
      - id: table_class
        type: u1
      - id: table_identifier
        type: u1
      - id: number_of_lengths
        type: u1
      - id: length_values
        type: u1
        repeat: expr
          value: number_of_lengths
      - id: values
        type: u1
        repeat: expr
          value: length_values
  component_parameter:
    seq:
      - id: component_id
        type: u1
      - id: sampling_factors
        type: u1
      - id: quantization_table
        type: u1
  raw:
    seq:
      - id: data
        type: u1
        repeat: until
          - marker