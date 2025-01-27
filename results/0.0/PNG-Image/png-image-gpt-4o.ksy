meta:
  id: png
  title: Portable Network Graphics
  file-extension: png
  xref:
    mime: image/png
  license: CC0-1.0
  endian: be
  encoding: utf-8

doc: |
  PNG (Portable Network Graphics) is a raster-graphics file-format that supports lossless data compression.

seq:
  - id: signature
    contents: [137, 80, 78, 71, 13, 10, 26, 10]
  - id: chunks
    type: chunk
    repeat: until
    repeat-until: _.is_end_chunk

types:
  chunk:
    seq:
      - id: length
        type: u4
      - id: type
        type: str
        size: 4
      - id: body
        size: length
        if: type != "IEND"
      - id: crc
        type: u4

    instances:
      is_end_chunk:
        value: type == "IEND"