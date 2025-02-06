type: seq
- id: signature
  type: bytes
  len: 8
  contents: [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
- id: chunks
  type: seq
  - id: chunk
    type: struct
    - id: length
      type: u4
    - id: type
      type: str4
    - id: data
      type: bytes
      len: length
    - id: crc
      type: u4
    - id: ihdr
      type: instance
      inst: ihdr_chunk
      if: type == "IHDR"
    - id: plte
      type: instance
      inst: plte_chunk
      if: type == "PLTE"
    - id: idat
      type: instance
      inst: idat_chunk
      if: type == "IDAT"
    - id: iend
      type: instance
      inst: iend_chunk
      if: type == "IEND"
    - id: text
      type: instance
      inst: text_chunk
      if: type == "tEXt"
    - id: ztxt
      type: instance
      inst: ztxt_chunk
      if: type == "zTXt"
    - id: itxt
      type: instance
      inst: itxt_chunk
      if: type == "iTXt"
    - id: chrm
      type: instance
      inst: chrm_chunk
      if: type == "cHRM"
    - id: gama
      type: instance
      inst: gama_chunk
      if: type == "gAMA"
    - id: iccp
      type: instance
      inst: iccp_chunk
      if: type == "iCCP"
    - id: sbit
      type: instance
      inst: sbit_chunk
      if: type == "sBIT"
    - id: srgb
      type: instance
      inst: srgb_chunk
      if: type == "sRGB"
    - id: bkgd
      type: instance
      inst: bkgd_chunk
      if: type == "bKGD"
    - id: hist
      type: instance
      inst: hist_chunk
      if: type == "hIST"
    - id: phys
      type: instance
      inst: phys_chunk
      if: type == "pHYs"
    - id: time
      type: instance
      inst: time_chunk
      if: type == "tIME"
    - id: trns
      type: instance
      inst: trns_chunk
      if: type == "tRNS"
    - id: unknown
      type: bytes
      len: length
      if: type != "IHDR" and type != "PLTE" and type != "IDAT" and type != "IEND" and type != "tEXt" and type != "zTXt" and type != "iTXt" and type != "cHRM" and type != "gAMA" and type != "iCCP" and type != "sBIT" and type != "sRGB" and type != "bKGD" and type != "hIST" and type != "pHYs" and type != "tIME" and type != "tRNS"

types:
  ihdr_chunk:
    type: struct
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
    type: seq
    - type: rgb
  rgb:
    type: struct
    - id: red
      type: u1
    - id: green
      type: u1
    - id: blue
      type: u1
  idat_chunk:
    type: bytes
    len: length
  iend_chunk:
    type: struct
  text_chunk:
    type: struct
    - id: keyword
      type: str
      encoding: ASCII
    - id: text
      type: str
      encoding: ASCII
  ztxt_chunk:
    type: struct
    - id: keyword
      type: str
      encoding: ASCII
    - id: compressed_text
      type: bytes
      len: length
  itxt_chunk:
    type: struct
    - id: keyword
      type: str
      encoding: ASCII
    - id: compression_flag
      type: u1
    - id: compression_method
      type: u1
    - id: language_tag
      type: str
      encoding: UTF-8
    - id: translated_keyword
      type: str
      encoding: UTF-8
    - id: text
      type: str
      encoding: UTF-8
  chrm_chunk:
    type: struct
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
    type: struct
    - id: gamma
      type: u4
  iccp_chunk:
    type: struct
    - id: profile_name
      type: str
      encoding: ASCII
    - id: compressed_profile
      type: bytes
      len: length
  sbit_chunk:
    type: struct
    - id: significant_bits
      type: bits
      len: 8
  srgb_chunk:
    type: struct
    - id: rendering_intent
      type: u1
  bkgd_chunk:
    type: struct
    - id: background_color
      type: u4
  hist_chunk:
    type: seq
    - type: u2
  phys_chunk:
    type: struct
    - id: pixels_per_unit_x
      type: u4
    - id: pixels_per_unit_y
      type: u4
    - id: unit_specifier
      type: u1
  time_chunk:
    type: struct
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
  trns_chunk:
    type: struct
    - id: alpha
      type: u2
