meta:
  id: png
  endian: be
types:
  chunk:
    seq:
      - id: length
        type: u4
      - id: type
        type: str
        size: 4
        encoding: ASCII
      - id: data
        type: bytes
        size: length
      - id: crc
        type: u4
  ihdr:
    seq:
      - id: width
        type: u4
      - id: height
        type: u4
      - id: bit_depth
        type: u1
      - id: color_type
        type: u1
      - id: compression_method
        type: u1
      - id: filter_method
        type: u1
      - id: interlace_method
        type: u1
  plte:
    seq:
      - id: palette
        type: palette_entry
        repeat: eos
  palette_entry:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1
  idat:
    seq:
      - id: data
        type: bytes
        size: length
  tRNS:
    seq:
      - id: transparency_data
        type: bytes
        size: length
  iend:
    seq:
      - id: null
        type: void
  png:
    seq:
      - id: signature
        type: bytes
        size: 8
        contents: [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
      - id: chunks
        type: chunk
        repeat: eos

