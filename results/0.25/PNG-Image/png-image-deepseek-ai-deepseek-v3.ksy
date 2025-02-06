meta:
  id: png
  file-extension: png
  endian: be
seq:
  - id: magic
    contents: "\x89PNG\r\n\x1a\n"
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
      - id: crc
        type: u4
    instances:
      ihdr:
        if: type == "IHDR"
        type: ihdr_chunk
      plte:
        if: type == "PLTE"
        type: plte_chunk
      idat:
        if: type == "IDAT"
        type: idat_chunk
      iend:
        if: type == "IEND"
        type: iend_chunk
      trns:
        if: type == "tRNS"
        type: trns_chunk
      chrm:
        if: type == "cHRM"
        type: chrm_chunk
      gama:
        if: type == "gAMA"
        type: gama_chunk
      iccp:
        if: type == "iCCP"
        type: iccp_chunk
      sbit:
        if: type == "sBIT"
        type: sbit_chunk
      srgb:
        if: type == "sRGB"
        type: srgb_chunk
      text:
        if: type == "tEXt"
        type: text_chunk
      ztxt:
        if: type == "zTXt"
        type: ztxt_chunk
      itxt:
        if: type == "iTXt"
        type: itxt_chunk
      bkgd:
        if: type == "bKGD"
        type: bkgd_chunk
      hist:
        if: type == "hIST"
        type: hist_chunk
      phys:
        if: type == "pHYs"
        type: phys_chunk
      splt:
        if: type == "sPLT"
        type: splt_chunk
      time:
        if: type == "tIME"
        type: time_chunk
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
        size-eos: true
  iend_chunk:
    seq: []
  trns_chunk:
    seq:
      - id: transparency_data
        size-eos: true
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
      - id: significant_bits
        size-eos: true
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
        type: strz
        encoding: ASCII
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
        type: strz
        encoding: UTF-8
  bkgd_chunk:
    seq:
      - id: background_color
        size-eos: true
  hist_chunk:
    seq:
      - id: histogram
        type: u2
        repeat: expr
        repeat-expr: _parent.length / 2
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
      - id: palette_entries
        size-eos: true
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