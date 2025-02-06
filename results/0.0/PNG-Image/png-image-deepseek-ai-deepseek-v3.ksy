meta:
  id: png
  title: Portable Network Graphics (PNG)
  file-extension: png
  license: MIT
  endian: be
seq:
  - id: magic
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
      chrm:
        if: type == "cHRM"
        type: chrm_chunk
      gama:
        if: type == "gAMA"
        type: gama_chunk
      hist:
        if: type == "hIST"
        type: hist_chunk
      iccp:
        if: type == "iCCP"
        type: iccp_chunk
      phys:
        if: type == "pHYs"
        type: phys_chunk
      sbit:
        if: type == "sBIT"
        type: sbit_chunk
      splt:
        if: type == "sPLT"
        type: splt_chunk
      srgb:
        if: type == "sRGB"
        type: srgb_chunk
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
      - id: data
        size: length
  iend_chunk:
    seq: []
  text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
      - id: text
        type: strz
        encoding: ISO8859-1
  ztxt_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
      - id: compression_method
        type: u1
      - id: compressed_text
        size: length - size(keyword) - 1
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
      - id: color
        type:
          switch-on: _root.chunks.find { it.type == "IHDR" }.color_type
          cases:
            0: u2
            2: rgb
            3: u1
            4: u2
            6: rgb
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
  hist_chunk:
    seq:
      - id: frequencies
        type: u2
        repeat: eos
  iccp_chunk:
    seq:
      - id: profile_name
        type: strz
        encoding: ASCII
      - id: compression_method
        type: u1
      - id: compressed_profile
        size: length - size(profile_name) - 1
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
      - id: significant_bits
        type:
          switch-on: _root.chunks.find { it.type == "IHDR" }.color_type
          cases:
            0: u1
            2: rgb
            3: u1
            4: u1
            6: rgb
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
        type: u2
      - id: green
        type: u2
      - id: blue
        type: u2
      - id: alpha
        type: u2
      - id: frequency
        type: u2
  srgb_chunk:
    seq:
      - id: rendering_intent
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