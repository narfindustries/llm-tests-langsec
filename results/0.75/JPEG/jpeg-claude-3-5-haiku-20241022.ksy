meta:
  id: jpeg
  file-extension: jpg
  endian: big
seq:
  - id: start_marker
    type: start_marker
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.marker == 0xd9
types:
  start_marker:
    seq:
      - id: soi_marker
        contents: [0xff, 0xd8]
  segment:
    seq:
      - id: marker
        type: u2
      - id: length
        type: u2
        if: marker != 0xd9
      - id: data
        size: length - 2
        if: marker != 0xd9 and marker != 0xda
    instances:
      is_standalone_marker:
        value: marker == 0xd9 or marker == 0xda