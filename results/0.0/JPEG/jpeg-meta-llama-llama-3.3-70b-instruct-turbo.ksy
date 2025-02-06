meta:
  id: jpeg
  endian: be
seq:
  - id: soi
    type: u1
  - id: marker
    type: u1
  - id: length
    type: u2
  - id: body
    type:
      switch-on: marker
      cases:
        192: 
          type:
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
                type: seq
                repeat: num_components
                seq:
                  - id: component_id
                    type: u1
                  - id: sampling_factors
                    type: u1
                  - id: quantization_table_id
                    type: u1
        193: 
          type:
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
                type: seq
                repeat: num_components
                seq:
                  - id: component_id
                    type: u1
                  - id: sampling_factors
                    type: u1
                  - id: quantization_table_id
                    type: u1
        194: 
          type:
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
                type: seq
                repeat: num_components
                seq:
                  - id: component_id
                    type: u1
                  - id: sampling_factors
                    type: u1
                  - id: quantization_table_id
                    type: u1
        195: 
          type:
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
                type: seq
                repeat: num_components
                seq:
                  - id: component_id
                    type: u1
                  - id: sampling_factors
                    type: u1
                  - id: quantization_table_id
                    type: u1
        196: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        197: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        198: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        199: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        200: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        201: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        202: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        203: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        204: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        205: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        206: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        207: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        208: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        209: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        210: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        211: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        212: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        213: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        214: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        215: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        216: 
          type:
            seq:
              - id: huffman_table_id
                type: u1
              - id: class
                type: u1
              - id: destination_id
                type: u1
              - id: num_codes
                type: u1
              - id: huffman_codes
                type: u1
                repeat: expr
        217: 
          type:
            seq:
              - id: eoi
                type: u1
                value: 217
        218: 
          type:
            seq:
              - id: num_components
                type: u1
              - id: components
                type: seq
                repeat: num_components
                seq:
                  - id: component_id
                    type: u1
                  - id: dc_huffman_table_id
                    type: u1
                  - id: ac_huffman_table_id
                    type: u1
              - id: restart_interval
                type: u2
              - id: entropy_coded_data
                type: u1
                repeat: expr
        219: 
          type:
            seq:
              - id: quantization_table_id
                type: u1
              - id: precision
                type: u1
              - id: quantization_table
                type: u1
                repeat: 64
        220: 
          type:
            seq:
              - id: quantization_table_id
                type: u1
              - id: precision
                type: u1
              - id: quantization_table
                type: u1
                repeat: 64
        221: 
          type:
            seq:
              - id: spectral_selection
                type: u1
              - id: num_components
                type: u1
              - id: components
                type: seq
                repeat: num_components
                seq:
                  - id: component_id
                    type: u1
                  - id: dc_huffman_table_id
                    type: u1
                  - id: ac_huffman_table_id
                    type: u1
        222: 
          type:
            seq:
              - id: spectral_selection
                type: u1
              - id: num_components
                type: u1
              - id: components
                type: seq
                repeat: num_components
                seq:
                  - id: component_id
                    type: u1
                  - id: dc_huffman_table_id
                    type: u1
                  - id: ac_huffman_table_id
                    type: u1
        223: 
          type:
            seq:
              - id: spectral_selection
                type: u1
              - id: num_components
                type: u1
              - id: components
                type: seq
                repeat: num_components
                seq:
                  - id: component_id
                    type: u1
                  - id: dc_huffman_table_id
                    type: u1
                  - id: ac_huffman_table_id
                    type: u1
        224: 
          type:
            seq:
              - id: restart_interval
                type: u2
        225: 
          type:
            seq:
              - id: restart_interval
                type: u2
        254: 
          type:
            seq:
              - id: identifier
                type: str
                size: 5
              - id: version
                type: u1
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
                type: u1
                repeat: expr
        255: 
          type:
            seq:
              - id: identifier
                type: str
                size: 5
              - id: version
                type: u1
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
                type: u1
                repeat: expr
        _:
          type: u1
          repeat: expr