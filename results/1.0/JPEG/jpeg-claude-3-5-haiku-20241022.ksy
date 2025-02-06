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
      - id: length
        type: u2
        if: marker != 0xFFD9
      
      - id: app0
        type: app0_segment
        if: marker == 0xFFE0
      
      - id: app1
        type: app1_segment
        if: marker == 0xFFE1
      
      - id: dqt
        type: dqt_segment
        if: marker == 0xFFDB
      
      - id: dht
        type: dht_segment
        if: marker == 0xFFC4
      
      - id: sof
        type: sof_segment
        if: marker == 0xFFC0 or marker == 0xFFC1 or marker == 0xFFC2
      
      - id: sos
        type: sos_segment
        if: marker == 0xFFDA
      
      - id: raw_data
        type: raw_bytes
        if: marker != 0xFFD9 and marker != 0xFFE0 and marker != 0xFFE1 and marker != 0xFFDB and marker != 0xFFC4 and marker != 0xFFC0 and marker != 0xFFC1 and marker != 0xFFC2 and marker != 0xFFDA

  raw_bytes:
    seq:
      - id: data
        size: _parent.length - 2
        type: u1[]

  app0_segment:
    seq:
      - id: identifier
        type: str
        size: 5
        encoding: ASCII
      - id: version
        type: u2
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
      - id: thumbnail_data
        type: u1[]
        size: thumbnail_width * thumbnail_height * 3
        if: thumbnail_width > 0 and thumbnail_height > 0

  app1_segment:
    seq:
      - id: exif_identifier
        type: str
        size: 6
        encoding: ASCII
      - id: exif_data
        type: u1[]
        size-eos: true

  dqt_segment:
    seq:
      - id: quantization_tables
        type: quantization_table
        repeat: until
        repeat-until: _io.is_eof

  quantization_table:
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
      - id: huffman_tables
        type: huffman_table
        repeat: until
        repeat-until: _io.is_eof

  huffman_table:
    seq:
      - id: table_class
        type: b4
      - id: table_destination_id
        type: b4
      - id: num_symbols_per_length
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: symbols
        type: u1
        repeat: expr
        repeat-expr: 16

  sof_segment:
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
      - id: num_components
        type: u1
      - id: components
        type: sos_component
        repeat: expr
        repeat-expr: num_components
      - id: start_spectral_selection
        type: u1
      - id: end_spectral_selection
        type: u1
      - id: approximation_bits
        type: u1
      - id: compressed_data
        type: u1[]
        size-eos: true

  sos_component:
    seq:
      - id: component_id
        type: u1
      - id: dc_huffman_table
        type: b4
      - id: ac_huffman_table
        type: b4