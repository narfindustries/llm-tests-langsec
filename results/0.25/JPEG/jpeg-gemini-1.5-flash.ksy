meta:
  id: jpeg-gemini-1
  title: JPEG Gemini 1.5 Flash
  endian: be

seq:
  - id: header
    type: header_t
  - id: segments
    type: seg_t*

types:
  header_t:
    seq:
      - id: magic
        type: u4
      - id: version
        type: u2
      - id: flags
        type: u2
      - id: data_size
        type: u4

  seg_t:
    seq:
      - id: type
        type: u2
      - id: size
        type: u4
      - id: data
        type: u4[size]


