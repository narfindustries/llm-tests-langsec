meta:
  id: png_image
  file-extension: png
  endian: be

seq:
  - id: magic
    contents: [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]
  - id: chunks
    type: chunk
    repeat: until
    repeat-until: _.type == 'IEND'

types:
  chunk:
    seq:
      - id: length
        type: u4
      - id: type
        type: str
        size: 4
        encoding: ascii
      - id: data
        size: length
      - id: crc
        type: u4

    instances:
      is_critical:
        value: (type.to_s[0].ord & 0x20) == 0

    enums:
      chunk_type:
        0x49484452: IHDR
        0x49444154: IDAT
        0x49454E44: IEND
        0x504C5445: PLTE
        0x74524E53: tRNS
        0x6348524D: cHRM
        0x67414D41: gAMA
        0x69434350: iCCP
        0x73424954: sBIT
        0x74455874: tEXt
        0x7A545874: zTXt
        0x69545874: iTXt
        0x624B4744: bKGD
        0x70485973: pHYs
        0x74494D45: tIME