meta:
  id: png
  title: Portable Network Graphics
  file-extension: png
  xref:
    mime: image/png
  license: CC0-1.0
  endian: be

seq:
  - id: signature
    contents: "\x89PNG\x0D\x0A\x1A\x0A"

  - id: chunks
    type: chunk
    repeat: until
    repeat-until: _.type == "IEND"

types:
  chunk:
    seq:
      - id: len
        type: u4

      - id: type
        type: str
        encoding: ASCII
        size: 4

      - id: body
        size: len

      - id: crc
        type: u4