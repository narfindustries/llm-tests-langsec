meta:
  id: png
  title: PNG (Portable Network Graphics)
  file-extension: png
  xref:
    mime: image/png
  license: CC0-1.0
  endian: be
  encoding: utf-8
doc: |
  Portable Network Graphics (PNG) is a raster-graphics file-format that supports lossless data compression.
seq:
  - id: signature
    contents: "\x89PNG\r\n\x1a\n"
  - id: chunks
    type: chunk
    repeat: eos

types:
  chunk:
    seq:
      - id: length
        type: u4
        doc: Length of the data field in the chunk.
      - id: type
        type: str
        size: 4
        doc: ASCII string indicating the chunk's type.
      - id: data
        size: length
        doc: Data of the chunk.
      - id: crc
        type: u4
        doc: CRC checksum of the type and data fields in the chunk.