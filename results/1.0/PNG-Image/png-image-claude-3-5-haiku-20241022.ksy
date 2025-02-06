meta:
  id: png
  file-extension: png
  endian: be

seq:
  - id: signature
    contents: [137, 80, 78, 71, 13, 10, 26, 10]
    
  - id: chunks
    type: chunk
    repeat: until
    repeat-until: _io.at_end or _.chunk_type == 'IEND'

types:
  chunk:
    seq:
      - id: length
        type: u4
      - id: chunk_type
        type: str
        size: 4
        encoding: ASCII
      - id: chunk_data
        size: length
        type:
          switch-on: chunk_type
          cases:
            "'IHDR'": ihdr_chunk
            "'PLTE'": plte_chunk
            "'tRNS'": trns_chunk
            "'gAMA'": gama_chunk
            "'cHRM'": chrm_chunk
            "'sRGB'": srgb_chunk
            "'iCCP'": iccp_chunk
            "'tEXt'": text_chunk
            "'zTXt'": ztxt_chunk
            "'iTXt'": itxt_chunk
            "'bKGD'": bkgd_chunk
            "'pHYs'": phys_chunk
            "'sBIT'": sbit_chunk
            "'sPLT'": splt_chunk
            "'hIST'": hist_chunk
            "'tIME'": time_chunk
            "'IDAT'": idat_chunk
            "'IEND'": iend_chunk
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
        enum: color_types
      - id: compression_method
        type: u1
      - id: filter_method
        type: u1
      - id: interlace_method
        type: u1
    enums:
      color_types:
        0: grayscale
        2: rgb
        3: palette
        4: grayscale_alpha
        6: rgba

  plte_chunk:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: _parent.length / 3

  rgb:
    seq:
      - id: r
        type: u1
      - id: g
        type: u1
      - id: b
        type: u1

  trns_chunk:
    seq:
      - id: alpha
        type: u1
        repeat: expr
        repeat-expr: _parent.length

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
        enum: rendering_intents
    enums:
      rendering_intents:
        0: perceptual
        1: relative_colorimetric
        2: saturation
        3: absolute_colorimetric

  iccp_chunk:
    seq:
      - id: profile_name
        type: strz
        encoding: ASCII
      - id: compression_method
        type: u1
      - id: compressed_profile
        type: str
        encoding: ASCII
        size-eos: true

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
        type: str
        encoding: ASCII
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
      - id: background
        type: str
        encoding: ASCII
        size-eos: true

  phys_chunk:
    seq:
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit
        type: u1
        enum: unit_types
    enums:
      unit_types:
        0: unknown
        1: meter

  sbit_chunk:
    seq:
      - id: significant_bits
        type: str
        encoding: ASCII
        size-eos: true

  splt_chunk:
    seq:
      - id: palette_name
        type: strz
        encoding: ASCII
      - id: sample_depth
        type: u1
      - id: entries
        type: palette_entry
        repeat: eos

  palette_entry:
    seq:
      - id: red
        type: u2
      - id: green
        type: u2
      - id: blue
        type: u2
      - id: alpha
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

  idat_chunk:
    seq:
      - id: data
        type: str
        encoding: ASCII
        size-eos: true

  iend_chunk:
    seq:
      - id: empty
        size: 0