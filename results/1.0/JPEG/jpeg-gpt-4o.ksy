meta:
  id: jpeg_gpt_4o
  file-extension: jpg
  endian: be

seq:
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.marker == 0xd9

types:
  segment:
    seq:
      - id: marker
        type: u2
      - id: length
        type: u2
        if: marker != 0xd8 and marker != 0xd9
      - id: data
        size: length - 2
        if: marker != 0xd8 and marker != 0xd9