meta:
  id: jpeg
  file-extension: jpg
  endian: big
seq:
  - id: start_of_file
    type: start_of_file
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.type == segment_type::end_of_image
types:
  start_of_file:
    seq:
      - id: soi_marker
        contents: [0xFF, 0xD8]
  segment:
    seq:
      - id: marker
        type: u1
        enum: segment_type
      - id: length
        type: u2
        if: marker != segment_type::end_of_image
      - id: data
        size: length - 2
        if: marker != segment_type::end_of_image
  
enums:
  segment_type:
    0xD8: start_of_image
    0xC0: start_of_frame
    0xC4: define_huffman_table
    0xDB: define_quantization_table
    0xDD: define_restart_interval
    0xDA: start_of_scan
    0xD9: end_of_image