meta:
  id: png
  file-extension: png
  endian: be
doc: |
  PNG (Portable Network Graphics) image format is an image file format that
  supports lossless data compression. PNG was created as an improved, non-patented
  replacement for Graphics Interchange Format (GIF), and is the most used
  lossless image compression format on the Internet.

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
        size: 4
        encoding: ASCII
      - id: data
        size: length
      - id: crc
        type: u4

    instances:
      is_ihdr:
        value: type == 'IHDR'
      is_plte:
        value: type == 'PLTE'
      is_idat:
        value: type == 'IDAT'
      is_iend:
        value: type == 'IEND'
      is_trns:
        value: type == 'tRNS'
      is_gama:
        value: type == 'gAMA'
      is_chrm:
        value: type == 'cHRM'
      is_srgb:
        value: type == 'sRGB'
      ihdr:
        pos: 0
        type: ihdr
        if: is_ihdr
      plte:
        pos: 0
        type: plte
        if: is_plte
      idat:
        pos: 0
        type: idat
        if: is_idat
      iend:
        pos: 0
        type: iend
        if: is_iend

  ihdr:
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

  plte:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: _parent.length / 3

  idat:
    seq:
      - id: data
        size-eos: true

  iend:
    seq: []

  rgb:
    seq:
      - id: r
        type: u1
      - id: g
        type: u1
      - id: b
        type: u1

  trns:
    seq:
      - id: alpha_values
        size: length
        type: u1
        repeat: expr
        repeat-expr: length

  gama:
    seq:
      - id: gamma
        type: u4

  chrm:
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

  srgb:
    seq:
      - id: rendering_intent
        type: u1
        enum: rendering_intent
        doc: |
          The rendering intent indicates the chromatic adaptation to be performed
          when the image is output on a medium other than the reference medium.

enums:
  rendering_intent:
    0: perceptual
    1: relative_colorimetric
    2: saturation
    3: absolute_colorimetric