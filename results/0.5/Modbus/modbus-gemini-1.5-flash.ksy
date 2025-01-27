meta:
  id: modbus-gemini-1
  title: Modbus Gemini 1.5 Flash
  homepage: https://github.com/kaitai-io/kaitai_struct_formats
  file-extension: .bin
  endian: be

seq:
  - id: header
    type: header
  - id: data
    type: data
    repeat: expr
    repeat-expr: header.data_length

types:
  header:
    seq:
      - id: magic
        type: u4
      - id: version
        type: u2
      - id: data_length
        type: u4

  data:
    seq:
      - id: type
        type: u1
      - id: length
        type: u1
      - id: payload
        type: u4
