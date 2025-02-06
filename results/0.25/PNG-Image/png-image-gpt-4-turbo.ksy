meta:
  id: png
  file-extension: png
  endian: be
  title: Portable Network Graphics (PNG)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  Portable Network Graphics (PNG) is a raster graphics file format that supports
  lossless data compression. PNG was developed as an improved, non-patented
  replacement for Graphics Interchange Format (GIF).

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
            '"zTXt"': ztxt_chunk
            '"iTXt"': itxt_chunk
            '"bKGD"': bkgd_chunk
            '"pHYs"': phys_chunk
            '"sBIT"': sbit_chunk
            '"sRGB"': srgb_chunk
            '"tIME"': time_chunk
            '"gAMA"': gama_chunk
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
        enum: color_type
      - id: compression_method
        type: u1
      - id: filter_method
        type: u1
      - id: interlace_method
        type: u1

  plte_chunk:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: _parent.length / 3

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
        encoding: UTF-8
      - id: text
        type: str
        size-eos: true
        encoding: UTF-8

  ztxt_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: UTF-8
      - id: compression_method
        type: u1
      - id: compressed_text
        size-eos: true

  itxt_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: UTF-8
      - id: compression_flag
        type: u1
      - id: compression_method
        type: u1
      - id: language_tag
        type: strz
        encoding: UTF-8
      - id: translated_keyword
        type: strz
        encoding: UTF-8
      - id: text
        type: str
        size-eos: true
        encoding: UTF-8

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
        enum: unit_specifier

  sbit_chunk:
    seq:
      - id: significant_bits
        size-eos: true

  srgb_chunk:
    seq:
      - id: rendering_intent
        type: u1
        enum: rendering_intent

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

  gama_chunk:
    seq:
      - id: gamma
        type: u4

  rgb:
    seq:
      - id: r
        type: u1
      - id: g
        type: u1
      - id: b
        type: u1

enums:
  color_type:
    0: grayscale
    2: truecolor
    3: indexed_color
    4: grayscale_alpha
    6: truecolor_alpha

  unit_specifier:
    0: unknown
    1: meter

  rendering_intent:
    0: perceptual
    1: relative_colorimetric
    2: saturation
    3: absolute_colorimetric