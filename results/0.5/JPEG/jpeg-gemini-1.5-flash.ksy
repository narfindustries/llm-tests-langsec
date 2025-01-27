meta:
  id: jpeg-gemini-1
  title: JPEG Gemini 1.5 Flash
  homepage: ""
  file-extension: jpg
  experimental: true

seq:
  - id: soi
    type: u2
    enum:
      0xFFD8: SOI
  - id: segments
    type: seq
    repeat: eos
    - id: segment_marker
      type: u2
      enum:
        0xFFE0: APP0
        0xFFE1: APP1
        0xFFDB: DQT
        0xFFC0: SOF0
        0xFFC4: DHT
        0xFFDA: SOS
        0xFFD9: EOI
    - id: segment_length
      type: u2be
    - id: segment_data
      type: ubyte
      repeat: expr
      expr: self.segment_length - 2
  - id: eoi
    type: u2
    enum:
      0xFFD9: EOI

types:
  u2:
    type: u2be
  u2be:
    type: u2
    endian: be
  ubyte:
    type: u1
