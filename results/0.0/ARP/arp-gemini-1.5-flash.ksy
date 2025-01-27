meta:
  id: arp-gemini-1
  title: ARP Gemini 1 Flash Memory
  homepage: ""
  authors: []
  license: ""
  compiler: 
    kaitai: 0.8

types:
  uint8:
    type: int
    size: 1

  uint16:
    type: int
    size: 2
    endian: be

  uint32:
    type: int
    size: 4
    endian: be

  uint64:
    type: int
    size: 8
    endian: be


seq:
  - id: header
    type: header_t
  - id: data
    type: seq
    size: lambda: self.header.data_len
    read: lambda x: x.read_bytes(self.header.data_len)


types:
  header_t:
    seq:
      - id: magic
        type: uint32
      - id: version
        type: uint16
      - id: data_len
        type: uint32

