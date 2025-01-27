meta:
  id: png
  file-extension: png
  endian: be
  title: Portable Network Graphics
  application: 
    - Multi-purpose
  xref:
    mime: image/png
    wikidata: Q178682

doc: |
  PNG (Portable Network Graphics) format stores graphical information in a compressed form.

seq:
  - id: signature
    contents: [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]

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
        type:
          switch-on: type
          cases:
            '"IHDR"': ihdr_chunk
            '"PLTE"': plte_chunk
            '"IDAT"': idat_chunk
            '"IEND"': iend_chunk
            '"tEXt"': text_chunk
            '"bKGD"': bkgd_chunk
            '"pHYs"': phys_chunk
            '"tIME"': time_chunk
      - id: crc
        type: u4

  ihdr_chunk:
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

  plte_chunk:
    seq:
      - id: palette
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

  idat_chunk:
    seq:
      - id: data
        size-eos: true

  iend_chunk:
    seq: []

  text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ISO-8859-1
      - id: text
        type: str
        size-eos: true
        encoding: ISO-8859-1

  bkgd_chunk:
    seq:
      - id: background
        size-eos: true

  phys_chunk:
    seq:
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit
        type: u1

  time_chunk:
    seq:
      - id: year
        type: u2
      - id: month
        type: u1
      - id: day
        type: u1
      - id: hour
        type: u1
      - id: minute
        type: u1
      - id: second
        type: u1
