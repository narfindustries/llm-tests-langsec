meta:
  id: jpeg
  file-extension: jpg
  endian: big
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
      
      - id: data
        size: length - 2
        if: marker != 0xFFD9 and marker != 0xFFD0 and marker != 0xFFD1
        type:
          switch-on: marker
          cases:
            0xFFE0: app0_segment
            0xFFE1: app1_segment
            0xFFC0: start_of_frame
            0xFFC4: huffman_table
            0xFFDB: quantization_table
            0xFFDA: start_of_scan
    
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
  
  app1_segment:
    seq:
      - id: identifier
        type: str
        size: 6
        encoding: ASCII
      - id: data
        type: exif_data
  
  exif_data:
    seq:
      - id: tiff_header
        type: tiff_header
  
  tiff_header:
    seq:
      - id: byte_order
        type: u2
      - id: version
        type: u2
      - id: ifd_offset
        type: u4
  
  start_of_frame:
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
  
  huffman_table:
    seq:
      - id: table_class
        type: b4
      - id: destination_id
        type: b4
      - id: lengths
        type: u1
        repeat: expr
        repeat-expr: 16
      - id: values
        type: u1
        repeat: expr
        repeat-expr: _root.calc_huffman_lengths(lengths)
  
  quantization_table:
    seq:
      - id: precision
        type: b4
      - id: table_id
        type: b4
      - id: table_data
        type: u1
        repeat: expr
        repeat-expr: precision == 0 ? 64 : 128
  
  start_of_scan:
    seq:
      - id: num_components
        type: u1
      - id: components
        type: scan_component
        repeat: expr
        repeat-expr: num_components
      - id: spectral_selection_start
        type: u1
      - id: spectral_selection_end
        type: u1
      - id: approximation_bit_pos
        type: u1
  
  scan_component:
    seq:
      - id: component_id
        type: u1
      - id: dc_ac_table_selector
        type: u1

instances:
  calc_huffman_lengths:
    type: u2
    params:
      - id: lengths
        type: u1[]
    value: >-
      lengths[0] + lengths[1] + lengths[2] + lengths[3] + 
      lengths[4] + lengths[5] + lengths[6] + lengths[7] + 
      lengths[8] + lengths[9] + lengths[10] + lengths[11] + 
      lengths[12] + lengths[13] + lengths[14] + lengths[15]