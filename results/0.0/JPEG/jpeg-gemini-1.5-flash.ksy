meta:
  id: jpeg-gemini-1
  title: JPEG Gemini 1.5 Flash
  homepage: ""
  authors: []
  license: ""
seq:
  - id: header
    type: seq
    contents:
      - id: magic
        type: u4
        enum:
          0x4A464946: jpeg
      - id: version
        type: u4
      - id: flags
        type: u4
      - id: data_size
        type: u8
  - id: data
    type: bytes
    size: root.header.data_size
