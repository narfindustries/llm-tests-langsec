meta:
  id: png
  file-extension: png
  endian: be

seq:
  - id: magic
    contents: [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
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
        encoding: UTF-8
      - id: data
        size: length
        type:
          switch-on: type
          cases:
            '"IHDR"': ihdr_chunk
            '"PLTE"': plte_chunk
            '"IDAT"': idat_chunk
            '"tRNS"': trns_chunk
            '"gAMA"': gama_chunk
            '"cHRM"': chrm_chunk
            '"sRGB"': srgb_chunk
            '"iCCP"': iccp_chunk
            '"tEXt"': text_chunk
            '"zTXt"': ztxt_chunk
            '"iTXt"': itxt_chunk
            '"bKGD"': bkgd_chunk
            '"pHYs"': phys_chunk
            '"sBIT"': sbit_chunk
            '"sPLT"': splt_chunk
            '"hIST"': hist_chunk
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
        repeat: eos

  rgb:
    seq:
      - id: r
        type: u1
      - id: g
        type: u1
      - id: b
        type: u1

  idat_chunk:
    seq:
      - id: data
        size-eos: true

  trns_chunk:
    seq:
      - id: data
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

  iccp_chunk:
    seq:
      - id: name
        type: strz
        encoding: UTF-8
      - id: compression_method
        type: u1
      - id: compressed_profile
        size-eos: true

  text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: UTF-8
      - id: text
        type: str
        encoding: UTF-8
        size-eos: true

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
        size-eos: true

  bkgd_chunk:
    seq:
      - id: data
        size-eos: true

  phys_chunk:
    seq:
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit_specifier
        type: u1

  sbit_chunk:
    seq:
      - id: data
        size-eos: true

  splt_chunk:
    seq:
      - id: palette_name
        type: strz
        encoding: UTF-8
      - id: sample_depth
        type: u1
      - id: entries
        size-eos: true

  hist_chunk:
    seq:
      - id: frequencies
        type: u2
        repeat: eos

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

enums:
  color_type:
    0: greyscale
    2: rgb
    3: palette
    4: greyscale_alpha
    6: rgb_alpha