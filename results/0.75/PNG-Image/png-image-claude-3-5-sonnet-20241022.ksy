meta:
  id: png
  file-extension: png
  endian: be

seq:
  - id: magic
    contents: [137, 80, 78, 71, 13, 10, 26, 10]
  - id: chunks
    type: chunk
    repeat: until
    repeat-until: _.type == "IEND"

types:
  chunk:
    seq:
      - id: length
        type: u4
      - id: type
        type: str
        size: 4
        encoding: ascii
      - id: data
        size: length
        type:
          switch-on: type
          cases:
            '"IHDR"': chunk_header
            '"PLTE"': chunk_palette
            '"IDAT"': chunk_data
            '"bKGD"': chunk_background
            '"cHRM"': chunk_chromaticity
            '"gAMA"': chunk_gamma
            '"hIST"': chunk_histogram
            '"iCCP"': chunk_icc_profile
            '"iTXt"': chunk_international_text
            '"pHYs"': chunk_physical
            '"sBIT"': chunk_significant_bits
            '"sPLT"': chunk_suggested_palette
            '"sRGB"': chunk_standard_rgb
            '"tEXt"': chunk_text
            '"tIME"': chunk_time
            '"tRNS"': chunk_transparency
            '"zTXt"': chunk_compressed_text
      - id: crc
        type: u4

  chunk_header:
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

  chunk_palette:
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

  chunk_data:
    seq:
      - id: data
        size: _parent.length

  chunk_background:
    seq:
      - id: data
        size: _parent.length
        type:
          switch-on: _root.chunks[0].data.as<chunk_header>.color_type
          cases:
            'color_type::greyscale': u2
            'color_type::greyscale_alpha': u2
            'color_type::truecolor': truecolor_background
            'color_type::truecolor_alpha': truecolor_background
            'color_type::indexed': u1

  truecolor_background:
    seq:
      - id: red
        type: u2
      - id: green
        type: u2
      - id: blue
        type: u2

  chunk_chromaticity:
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

  chunk_gamma:
    seq:
      - id: gamma_int
        type: u4

  chunk_histogram:
    seq:
      - id: frequencies
        type: u2
        repeat: eos

  chunk_icc_profile:
    seq:
      - id: profile_name
        type: strz
        encoding: ascii
      - id: compression_method
        type: u1
      - id: compressed_profile
        size: _parent.length - profile_name.length - 2

  chunk_international_text:
    seq:
      - id: keyword
        type: strz
        encoding: ascii
      - id: compression_flag
        type: u1
      - id: compression_method
        type: u1
      - id: language_tag
        type: strz
        encoding: ascii
      - id: translated_keyword
        type: strz
        encoding: utf8
      - id: text
        size: _parent.length - keyword.length - language_tag.length - translated_keyword.length - 5

  chunk_physical:
    seq:
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit
        type: u1
        enum: phys_unit

  chunk_significant_bits:
    seq:
      - id: data
        size: _parent.length

  chunk_suggested_palette:
    seq:
      - id: palette_name
        type: strz
        encoding: ascii
      - id: sample_depth
        type: u1
      - id: entries
        type:
          switch-on: sample_depth
          cases:
            8: splt_entry8
            16: splt_entry16
        repeat: eos

  splt_entry8:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1
      - id: alpha
        type: u1
      - id: frequency
        type: u2

  splt_entry16:
    seq:
      - id: red
        type: u2
      - id: green
        type: u2
      - id: blue
        type: u2
      - id: alpha
        type: u2
      - id: frequency
        type: u2

  chunk_standard_rgb:
    seq:
      - id: rendering_intent
        type: u1
        enum: srgb_intent

  chunk_text:
    seq:
      - id: keyword
        type: strz
        encoding: ascii
      - id: text_data
        type: str
        size: _parent.length - keyword.length - 1
        encoding: ascii

  chunk_time:
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

  chunk_transparency:
    seq:
      - id: data
        size: _parent.length

  chunk_compressed_text:
    seq:
      - id: keyword
        type: strz
        encoding: ascii
      - id: compression_method
        type: u1
      - id: compressed_text
        size: _parent.length - keyword.length - 2

enums:
  color_type:
    0: greyscale
    2: truecolor
    3: indexed
    4: greyscale_alpha
    6: truecolor_alpha

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