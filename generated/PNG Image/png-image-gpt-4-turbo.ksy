meta:
  id: png_image
  file-extension: png
  endian: le
doc: |
  PNG (Portable Network Graphics) is an image format that supports lossless compression.
  This specification covers the basic structure of a PNG file.
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
        size: 4
        encoding: ASCII
      - id: data
        size: length
      - id: crc
        type: u4
    instances:
      is_ihdr:
        value: type == 'IHDR'
      is_idat:
        value: type == 'IDAT'
      is_iend:
        value: type == 'IEND'