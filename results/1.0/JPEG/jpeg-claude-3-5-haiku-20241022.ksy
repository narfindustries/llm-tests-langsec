meta:
  id: jpeg
  file-extension: jpg
  endian: be
seq:
  - id: segments
    type: jpeg_segment
    repeat: until
    repeat-until: _.marker != 0xff
types:
  jpeg_segment:
    seq:
      - id: marker
        type: u1
      - id: length
        type: u2
        if: marker != 0xd8 and marker != 0xd9
      - id: data
        size: length - 2
        if: marker != 0xd8 and marker != 0xd9 and marker != 0x01
enums:
  marker_types:
    0xd8: start_of_image
    0xc0: start_of_frame_baseline
    0xc2: start_of_frame_progressive
    0xc4: define_huffman_table
    0xdb: define_quantization_table
    0xdd: define_restart_interval
    0xda: start_of_scan
    0xd9: end_of_image