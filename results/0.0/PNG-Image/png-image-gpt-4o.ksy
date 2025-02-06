meta:
  id: png
  title: PNG (Portable Network Graphics)
  file-extension: png
  xref:
    iso: ISO/IEC 15948:2004
  license: CC0-1.0
  endian: be

seq:
  - id: signature
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
      - id: compressed_data
        size: _parent.length
        type: bytes

  iend_chunk:
    seq: []

  trns_chunk:
    seq:
      - id: transparency_data
        size: _parent.length
        type: bytes

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
        size: _parent.length - profile_name.length - 2
        type: bytes

  sbit_chunk:
    seq:
      - id: significant_bits
        size: _parent.length
        type: bytes

  srgb_chunk:
    seq:
      - id: rendering_intent
        type: u1

  text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ISO-8859-1
      - id: text
        size: _parent.length - keyword.length - 1
        type: str
        encoding: ISO-8859-1

  ztxt_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ISO-8859-1
      - id: compression_method
        type: u1
      - id: compressed_text
        size: _parent.length - keyword.length - 2
        type: bytes

  itxt_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ISO-8859-1
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
        size: _parent.length - keyword.length - language_tag.length - translated_keyword.length - 5
        type: str
        encoding: UTF-8

  bkgd_chunk:
    seq:
      - id: background_color
        size: _parent.length
        type: bytes

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
        encoding: ISO-8859-1
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