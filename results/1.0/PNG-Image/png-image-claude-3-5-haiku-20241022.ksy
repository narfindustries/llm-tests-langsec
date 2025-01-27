meta:
  id: png_image
  file-extension: png
  endian: be

seq:
  - id: signature
    contents: [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

  - id: ihdr_chunk
    type: chunk
    doc: Image header chunk

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
        encoding: ASCII
      - id: data
        size: length
        type: 
          switch-on: type
          cases:
            "'IHDR'": ihdr_data
            "'IDAT'": idat_data
            "'PLTE'": plte_data
            _: raw_data
      - id: crc
        type: u4

  ihdr_data:
    seq:
      - id: width
        type: u4
      - id: height
        type: u4
      - id: bit_depth
        type: u1
      - id: color_type
        type: u1
      - id: compression
        type: u1
      - id: filter
        type: u1
      - id: interlace
        type: u1

  plte_data:
    seq:
      - id: palette_entries
        type: palette_entry
        repeat: expr
        repeat-expr: _parent.length / 3

  palette_entry:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

  idat_data:
    seq:
      - id: data
        size-eos: true

  raw_data:
    seq:
      - id: content
        size-eos: true