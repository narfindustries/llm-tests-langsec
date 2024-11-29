type: png_file

signature:
  type: bytes
  size: 8
  enum:
    - 0x89
    - 0x50
    - 0x4E
    - 0x47
    - 0x0D
    - 0x0A
    - 0x1A
    - 0x0A

chunks:
  type: seq
  
  items:
    type: png_chunk

type: png_chunk

length:
  type: u4

type:
  type: strz
  size: 4

data:
  type: bytes
  size: lambda: length

crc:
  type: u4