meta:
  id: png
  file-extension: png
  endian: be

seq:
  - id: magic
    contents: [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]
  - id: chunks
    type: chunk
    repeat: until
    repeat-until: _.chunk_type == "IEND"

types:
  chunk:
    seq:
      - id: len
        type: u4
      - id: chunk_type
        type: str
        size: 4
        encoding: ASCII
      - id: chunk_data
        size: len
        type:
          switch-on: chunk_type
          cases:
            '"IHDR"': ihdr
            '"PLTE"': plte
            '"IDAT"': idat
            '"bKGD"': bkgd
            '"cHRM"': chrm
            '"gAMA"': gama
            '"hIST"': hist
            '"iCCP"': iccp
            '"iTXt"': itxt
            '"pHYs"': phys
            '"sBIT"': sbit
            '"sPLT"': splt
            '"sRGB"': srgb
            '"tEXt"': text
            '"tIME"': time
            '"tRNS"': trns
            '"zTXt"': ztxt
      - id: crc
        type: u4

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
        enum: color_type
      - id: compression_method
        type: u1
      - id: filter_method
        type: u1
      - id: interlace_method
        type: u1
        enum: interlace_method

  plte:
    seq:
      - id: entries
        type: rgb
        repeat: eos

  rgb:
    seq:
      - id: r
        type: u1
      - id: g
        type: u1
      - id: b
        type: u1

  idat:
    seq:
      - id: data
        size-eos: true

  bkgd:
    seq:
      - id: data
        size-eos: true

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

  gama:
    seq:
      - id: gamma_int
        type: u4

  hist:
    seq:
      - id: frequencies
        type: u2
        repeat: eos

  iccp:
    seq:
      - id: profile_name
        type: strz
        encoding: ASCII
      - id: compression_method
        type: u1
      - id: compressed_profile
        size-eos: true

  itxt:
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
        encoding: ASCII
      - id: translated_keyword
        type: strz
        encoding: UTF-8
      - id: text
        type: str
        encoding: UTF-8
        size-eos: true

  phys:
    seq:
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit
        type: u1
        enum: phys_unit

  sbit:
    seq:
      - id: data
        size-eos: true

  splt:
    seq:
      - id: palette_name
        type: strz
        encoding: ASCII
      - id: sample_depth
        type: u1
      - id: entries
        size-eos: true

  srgb:
    seq:
      - id: rendering_intent
        type: u1
        enum: srgb_intent

  text:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
      - id: text
        type: str
        encoding: ASCII
        size-eos: true

  time:
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

  trns:
    seq:
      - id: data
        size-eos: true

  ztxt:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
      - id: compression_method
        type: u1
      - id: compressed_text
        size-eos: true

enums:
  color_type:
    0: grayscale
    2: rgb
    3: palette
    4: grayscale_alpha
    6: rgb_alpha

  interlace_method:
    0: none
    1: adam7

  phys_unit:
    0: unknown
    1: meter

  srgb_intent:
    0: perceptual
    1: relative_colorimetric
    2: saturation
    3: absolute_colorimetric