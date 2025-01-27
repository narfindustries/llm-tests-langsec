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
    repeat-until: _.is_last

types:
  chunk:
    seq:
      - id: len
        type: u4
      - id: type
        type: str
        size: 4
      - id: body
        size: len
        if: not is_known_type
      - id: crc
        type: u4
    instances:
      is_last:
        value: type == "IEND"
      is_known_type:
        value: type in ["IHDR", "PLTE", "IDAT", "IEND"]
    types:
      ihdr_body:
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
      plte_body:
        seq:
          - id: entries
            type: rgb
            repeat: expr
            repeat-expr: len / 3
      idat_body:
        seq:
          - id: data
            size: len
      iend_body:
        seq: []

      rgb:
        seq:
          - id: r
            type: u1
          - id: g
            type: u1
          - id: b
            type: u1