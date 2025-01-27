meta:
  id: png-image
  title: PNG Image
  file-extension: png
  endian: le
seq:
  - id: magic
    contents: [137, 80, 78, 71, 13, 10, 26, 10]
  - id: ihdr
    type: chunk
  - id: chunks
    type: chunk
    repeat: until
    repeat-until: id == 0x49454e44
  - id: iend
    contents: [73, 69, 78, 68, 0x00, 0x00, 0x00, 0x00]
types:
  chunk:
    seq:
      - id: length
        size: 4
      - id: id
        size: 4
      - id: data
        size: length
      - id: crc
        size: 4