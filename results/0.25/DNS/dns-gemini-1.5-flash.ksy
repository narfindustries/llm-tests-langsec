meta:
  id: dns-gemini-1
  title: DNS Gemini 1.5 Flash
  homepage: ""
  file-extension: ".ksy"
  endian: be

types:
  uint8:
    type: int8
  uint16:
    type: int16
  uint32:
    type: int32
  uint64:
    type: int64

seq:
  - id: magic
    type: str
    size: 4
    doc: "Magic number"
  - id: version
    type: uint32
    doc: "Version number"
  - id: header_size
    type: uint32
    doc: "Size of the header"
  - id: data_size
    type: uint32
    doc: "Size of the data"
  - id: data
    type: bytes
    size: data_size
    doc: "Data section"

