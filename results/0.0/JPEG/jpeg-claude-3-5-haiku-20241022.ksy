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
        type: u2be
      - id: length
        type: u2be
        if: marker != 0xFFD9
      
      - id: data
        size: length - 2
        type: 
          switch-on: marker
          cases:
            0xFFE0: app0_segment
            0xFFE1: app1_segment
            0xFFDB: dqt_segment
            0xFFC0: baseline_sof_segment
            0xFFC2: progressive_sof_segment
            0xFFC4: dht_segment
            0xFFDD: dri_segment
            0xFFDA: sos_segment
    
  app0_segment:
    seq:
      - id: identifier
        type: strz
        size: 5
      - id: version
        type: u2be
      - id: density_units
        type: u1
      - id: x_density
        type: u2be
      - id: y_density
        type: u2be
      - id: thumbnail_width
        type: u1
      - id: thumbnail_height
        type: u1
  
  app1_segment:
    seq:
      - id: identifier
        type: strz
        size: 5
      - id: data
        type: bytes
  
  dqt_segment:
    seq:
      - id: tables
        type: quantization_table
        repeat: expr
        repeat-expr: (_parent.length - 2) / 65
    
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
  
  baseline_sof_segment:
    seq:
      - id: precision
        type: u1
      - id: height
        type: u2be
      - id: width
        type: u2be
      - id: num_components
        type: u1
      - id: components
        type: sof_component
        repeat: expr
        repeat-expr: num_components
    
  progressive_sof_segment:
    seq:
      - id: precision
        type: u1
      - id: height
        type: u2be
      - id: width
        type: u2be
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
  
  dht_segment:
    seq:
      - id: tables
        type: huffman_table
        repeat: expr
        repeat-expr: (_parent.length - 2) / 17
    
  huffman_table:
    seq:
      - id: table_class
        type: b4
      - id: table_destination
        type: b4
      - id: lengths
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: values
        type: u1
        repeat: expr
        repeat-expr: _root.calc_huffman_values_length(lengths)
  
  dri_segment:
    seq:
      - id: restart_interval
        type: u2be
  
  sos_segment:
    seq:
      - id: num_components
        type: u1
      - id: components
        type: sos_component
        repeat: expr
        repeat-expr: num_components
      - id: spectral_selection_start
        type: u1
      - id: spectral_selection_end
        type: u1
      - id: approximation_bit
        type: u1
  
  sos_component:
    seq:
      - id: component_selector
        type: u1
      - id: dc_ac_huffman_tables
        type: u1

instances:
  calc_huffman_values_length:
    params:
      - id: lengths
        type: u1[]
    value: >-
      lengths[0] + lengths[1] + lengths[2] + lengths[3] + lengths[4] + lengths[5] + 
      lengths[6] + lengths[7] + lengths[8] + lengths[9] + lengths[10] + lengths[11] + 
      lengths[12] + lengths[13] + lengths[14] + lengths[15]