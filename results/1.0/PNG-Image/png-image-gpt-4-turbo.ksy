meta:
  id: png
  file-extension: png
  endian: le
  license: CC0-1.0
  title: PNG Image Format
  ks-version: 0.9

doc: |
  PNG (Portable Network Graphics) is an image format supporting lossless compression.
  PNG is intended to replace GIF and can be used without patent restrictions.

doc-ref: http://www.libpng.org/pub/png/spec/1.2/PNG-Contents.html

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
      - id: body
        size: length
        type:
          switch-on: type
          cases:
            '"IHDR"': ihdr_chunk
            '"PLTE"': plte_chunk
            '"IDAT"': idat_chunk
            '"IEND"': iend_chunk
            '"tEXt"': text_chunk
      - id: crc
        type: u4

    types:
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
            enum: color_type
          - id: compression_method
            type: u1
          - id: filter_method
            type: u1
          - id: interlace_method
            type: u1
        enums:
          color_type:
            0: greyscale
            2: truecolor
            3: indexed
            4: greyscale_alpha
            6: truecolor_alpha

      plte_chunk:
        seq:
          - id: entries
            type: plte_entry
            repeat: expr
            repeat-expr: _parent.length / 3

      plte_entry:
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
            size: length

      iend_chunk:
        # IEND chunks have empty bodies.
        size: 0

      text_chunk:
        seq:
          - id: keyword
            type: strz
            encoding: ISO-8859-1
          - id: text
            type: str
            encoding: ISO-8859-1
            size-eos: true