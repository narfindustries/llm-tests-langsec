meta:
  id: png
  file-extension: png
  endian: le
  license: CC0-1.0
  ks-version: 0.9

doc: |
  PNG (Portable Network Graphics) is an image format that supports lossless compression and varying levels of transparency. This specification covers the critical parts of the PNG format.

seq:
  - id: signature
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
        size: length
      - id: crc
        type: u4

    instances:
      is_ihdr:
        value: type == 'IHDR'
      is_plte:
        value: type == 'PLTE'
      is_idat:
        value: type == 'IDAT'
      is_iend:
        value: type == 'IEND'