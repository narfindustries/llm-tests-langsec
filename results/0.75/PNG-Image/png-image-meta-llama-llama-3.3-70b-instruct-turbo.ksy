meta:
  id: png_image
  file-extension: png
  endian: le
seq:
  - id: magic
    contents: [137, 80, 78, 71, 13, 10, 26, 10]
  - id: ihdr
    type: chunk
  - id: chunks
    type: chunk
    repeat: until бар
    repeat-until: type == "iend"
  - id: iend
    type: chunk
types:
  chunk:
    seq:
      - id: length
        size: 4
      - id: type
        size: 4
      - id: data
        size: length
      - id: crc
        size: 4