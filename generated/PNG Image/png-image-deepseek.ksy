meta:
  id: png
  file-extension: png
  endian: big
seq:
  - id: signature
    type: u1
    repeat: expr
    repeat-expr: 8
    contents: [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
  - id: chunks
    type: chunk
    repeat: eos
types:
  chunk:
    seq:
      - id: length
        type: u4
      - id: type
        type: str
        encoding: ASCII
        size: 4
      - id: data
        type: u1
        repeat: expr
        repeat-expr: length
      - id: crc
        type: u4
    instances:
      type_enum:
        value: type
        enum: chunk_type
enums:
  chunk_type:
    0x49484452: IHDR
    0x49454E44: IEND
    0x504C5445: PLTE
    0x49444154: IDAT
    0x74524E53: tRNS
    0x67414D41: gAMA
    0x6348524D: cHRM
    0x70485973: pHYs
    0x73424954: sBIT
    0x73524742: sRGB
    0x624B4744: bKGD
    0x68495354: hIST
    0x70434D41: pCAL
    0x7350414C: sPLT
    0x74494D45: tIME
    0x69545874: iTXt
    0x74455874: tEXt
    0x7A545874: zTXt
    0x69454E44: iEND
    0x69484452: iHDR
    0x69504C54: iPLT
    0x69444154: iDAT
    0x69524E53: iRNS
    0x6947414D: iGAM
    0x69434852: iCHR
    0x69504859: iPHY
    0x69534249: iSBI
    0x69535247: iSRG
    0x69424B47: iBKD
    0x69484953: iHIS
    0x6950434D: iPAL
    0x69535041: iSPL
    0x6954494D: iTIM
    0x69545874: iTXt
    0x69455874: iEXt
    0x695A5458: iZTX
instances:
  IHDR:
    value: chunks[0]
    if: chunks[0].type_enum == chunk_type::IHDR
  IEND:
    value: chunks[-1]
    if: chunks[-1].type_enum == chunk_type::IEND