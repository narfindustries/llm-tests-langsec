meta:
  id: png-image-gemini-1
  endian: be

types:
  png_chunk:
    seq:
      - id: length
        type: u4
      - id: type
        type: str4
      - id: data
        type: bytes
        size: length
      - id: crc
        type: u4

  png_ihdr:
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

  png_idat:
    seq:
      - id: data
        type: bytes
        size: length


  png_iend:
    seq:
      - id: data
        type: bytes
        size: 0


seq:
  - id: signature
    type: str8
    enum:
      0: 0x89504e470d0a1a0a
  - id: chunks
    type: seq
    repeat: eos
    elemType: png_chunk


