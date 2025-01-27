meta:
  id: dns-gemini-1
  title: DNS Gemini 1.5 Flash
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
  - id: version
    type: uint32
  - id: header_size
    type: uint32
  - id: data_size
    type: uint64
  - id: data
    type: bytes
    size: data_size

