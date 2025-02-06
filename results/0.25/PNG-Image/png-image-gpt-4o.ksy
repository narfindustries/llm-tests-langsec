meta:
  id: png
  title: Portable Network Graphics
  file-extension: png
  xref:
    iso: ISO/IEC 15948:2004
  license: CC0-1.0
  endian: be

seq:
  - id: signature
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
        encoding: ASCII
      - id: data
        size: length
        type:
          switch-on: type
          cases:
            '"IHDR"': ihdr_chunk
            '"PLTE"': plte_chunk
            '"IDAT"': idat_chunk
            '"IEND"': iend_chunk
            '"tRNS"': trns_chunk
            '"cHRM"': chrm_chunk
            '"gAMA"': gama_chunk
            '"iCCP"': iccp_chunk
            '"sBIT"': sbit_chunk
            '"sRGB"': srgb_chunk
            '"tEXt"': text_chunk
            '"zTXt"': ztxt_chunk
            '"iTXt"': itxt_chunk
            '"bKGD"': bkgd_chunk
            '"pHYs"': phys_chunk
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
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

  idat_chunk:
    seq:
      - id: image_data
        size-eos: true

  iend_chunk:
    seq: []

  trns_chunk:
    seq:
      - id: transparency
        type: u1
        repeat: eos

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

  gama_chunk:
    seq:
      - id: gamma
        type: u4

  iccp_chunk:
    seq:
      - id: profile_name
        type: strz
        encoding: ASCII
      - id: compression_method
        type: u1
      - id: compressed_profile
        size-eos: true

  sbit_chunk:
    seq:
      - id: bits
        type: u1
        repeat: eos

  srgb_chunk:
    seq:
      - id: rendering_intent
        type: u1

  text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
      - id: text
        type: str
        size-eos: true
        encoding: UTF-8

  ztxt_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
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
        encoding: ASCII
      - id: translated_keyword
        type: strz
        encoding: UTF-8
      - id: text
        type: str
        size-eos: true
        encoding: UTF-8

  bkgd_chunk:
    seq:
      - id: background_color
        type: u1
        repeat: eos

  phys_chunk:
    seq:
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit_specifier
        type: u1

  splt_chunk:
    seq:
      - id: palette_name
        type: strz
        encoding: ASCII
      - id: sample_depth
        type: u1
      - id: entries
        type: splt_entry
        repeat: eos

  splt_entry:
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