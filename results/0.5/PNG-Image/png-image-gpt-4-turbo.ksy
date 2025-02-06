meta:
  id: png
  file-extension: png
  endian: be
  title: Portable Network Graphics (PNG)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  PNG is a popular image format that supports lossless data compression.
  PNG is designed to work well in online viewing applications, like web browsers,
  and can be fully streamable with a progressive display option.
  PNG files consist of chunks, where each chunk declares its own size,
  enabling the decoder to skip uninteresting (or unknown) chunks effectively.

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
            '"gAMA"': gama_chunk
            '"cHRM"': chrm_chunk
            '"sRGB"': srgb_chunk
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
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: _parent.length / 3

  idat_chunk:
    seq:
      - id: data
        size: _parent.length

  iend_chunk:
    seq: []

  text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: UTF-8
      - id: text
        type: str
        encoding: UTF-8
        size-eos: true

  gama_chunk:
    seq:
      - id: gamma
        type: u4

  chrm_chunk:
    seq:
      - id: white_point_x
        type: u4
      - id: white_point_y
        type: u4
      - id: red_x
        type: u4
      - id: red_y
        type: u4
      - id: green_x
        type: u4
      - id: green_y
        type: u4
      - id: blue_x
        type: u4
      - id: blue_y
        type: u4

  srgb_chunk:
    seq:
      - id: rendering_intent
        type: u1

  bkgd_chunk:
    seq:
      - id: background
        size: _parent.length

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

  rgb:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1