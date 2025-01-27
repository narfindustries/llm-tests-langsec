meta:
  id: png-image-gemini-1
  endian: be

seq:
  - id: signature
    type: u4
    enum:
      0x89504e47: png
  - id: ihdr
    type: ihdr
  - id: idat
    type: idat_list
  - id: iend
    type: iend

types:
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

  idat_list:
    type: seq
    repeat: eos
    contents:
      - id: idat_chunk
          type: idat

  idat:
    seq:
      - id: length
        type: u4
      - id: type
        type: u4
      - id: data
        type: bytes
        size: length
      - id: crc
        type: u4

  iend:
    seq:
      - id: length
        type: u4
      - id: type
        type: u4
      - id: data
        type: bytes
        size: 0
      - id: crc
        type: u4

