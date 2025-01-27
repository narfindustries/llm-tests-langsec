meta:
  id: png
  file-extension: png
  endian: be
seq:
  - id: header
    type: header
  - id: chunks
    type: chunk
    repeat: eos
types:
  header:
    seq:
      - id: magic
        contents: [137, 80, 78, 71, 13, 10, 26, 10]
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
            "'IHDR'": ihdr_chunk
            "'PLTE'": plte_chunk
            "'IDAT'": idat_chunk
            "'IEND'": iend_chunk
            "'tRNS'": trns_chunk
            "'gAMA'": gama_chunk
            "'cHRM'": chrm_chunk
            "'sRGB'": srgb_chunk
            "'iCCP'": iccp_chunk
            "'tEXt'": text_chunk
            "'zTXt'": ztxt_chunk
            "'bKGD'": bkgd_chunk
            "'pHYs'": phys_chunk
            "'tIME'": time_chunk
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
  idat_chunk:
    seq:
      - id: data
        size-eos: true
  iend_chunk:
    seq: []
  trns_chunk:
    seq:
      - id: alpha_values
        type: u1
        repeat: eos
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
      - id: profile_name
        type: strz
        encoding: ascii
      - id: compression_method
        type: u1
      - id: compressed_profile
        size-eos: true
  text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ascii
      - id: text
        type: strz
        encoding: ascii
  ztxt_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ascii
      - id: compression_method
        type: u1
      - id: compressed_text
        size-eos: true
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
      - id: unit
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