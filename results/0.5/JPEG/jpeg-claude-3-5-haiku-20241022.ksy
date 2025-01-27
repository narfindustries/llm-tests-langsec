meta:
  id: jpeg
  file-extension: jpg
  endian: big
seq:
  - id: start_marker
    type: u2
    enum: marker_type
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.marker == marker_type::end_of_image
types:
  segment:
    seq:
      - id: marker
        type: u2
        enum: marker_type
      - id: length
        type: u2
        if: marker != marker_type::start_of_image and marker != marker_type::end_of_image
      - id: data
        size: length - 2
        if: marker != marker_type::start_of_image and marker != marker_type::end_of_image
enums:
  marker_type:
    0xD8: start_of_image
    0xC0: start_of_frame
    0xC2: start_of_frame_progressive
    0xC4: define_huffman_table
    0xDB: define_quantization_table
    0xDD: define_restart_interval
    0xDA: start_of_scan
    0xD9: end_of_image
    0xE0: application_specific