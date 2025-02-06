seq:
  - id: magic
    type: byte[8]
    enum: [137, 80, 78, 71, 13, 10, 26, 10]

  - id: chunks
    type: chunk
    repeat: until eos

types:
  chunk:
    seq:
      - id: length
        type: u4
      - id: type
        type: str(4, encoding: ASCII)
      - id: data
        type:
          switch on: type
          cases:
            - case: 'IHDR'
              type: ihdr_data
            - case: 'PLTE'
              type: plte_data
            - case: 'IDAT'
              type: idat_data
            - case: 'IEND'
              type: iend_data
            - case: 'tEXt'
              type: text_data
            - case: 'zTXt'
              type: ztxt_data
            - case: 'iTXt'
              type: itxt_data
            - case: 'tIME'
              type: time_data
            - case: 'pHYs'
              type: phys_data
            - case: 'sBIT'
              type: sbit_data
            - case: 'cHRM'
              type: chrm_data
            - case: 'gAMA'
              type: gama_data
            - case: 'bKGD'
              type: bkgd_data
            - case: 'hIST'
              type: hist_data
            - case: 'sPLT'
              type: splt_data
            - case: 'sRGB'
              type: srgb_data
            - case: 'iCCP'
              type: iccp_data
            - case: 'tRNS'
              type: trns_data
      - id: crc
        type: u4

  ihdr_data:
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

  plte_data:
    seq:
      - id: palette
        type: rgb
        repeat: expr
        expr: length // 3

  idat_data:
    seq:
      - id: data
        type: bytes
        size: length

  iend_data:
    seq: []

  text_data:
    seq:
      - id: keyword
        type: str(1..79, encoding: ASCII)
      - id: null_byte
        type: u1
        enum: [0]
      - id: text
        type: str(null_terminated: true, encoding: ASCII)

  ztxt_data:
    seq:
      - id: keyword
        type: str(1..79, encoding: ASCII)
      - id: null_byte
        type: u1
        enum: [0]
      - id: compression_method
        type: u1
      - id: compressed_text
        type: bytes

  itxt_data:
    seq:
      - id: keyword
        type: str(1..79, encoding: ASCII)
      - id: null_byte
        type: u1
        enum: [0]
      - id: compression_flag
        type: u1
      - id: compression_method
        type: u1
      - id: language_tag
        type: str(null_terminated: true, encoding: ASCII)
      - id: translated_keyword
        type: str(null_terminated: true, encoding: ASCII)
      - id: text
        type: str(null_terminated: true, encoding: ASCII)

  time_data:
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

  phys_data:
    seq:
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit
        type: u1

  sbit_data:
    seq:
      - id: significant_bits
        type: bytes

  chrm_data:
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

  gama_data:
    seq:
      - id: gamma
        type: u4

  bkgd_data:
    seq:
      - id: background
        type: bytes

  hist_data:
    seq:
      - id: histogram
        type: bytes

  splt_data:
    seq:
      - id: palette_name
        type: str(null_terminated: true, encoding: ASCII)
      - id: depth
        type: u1
      - id: entries
        type: bytes

  srgb_data:
    seq:
      - id: rendering_intent
        type: u1

  iccp_data:
    seq:
      - id: profile_name
        type: str(null_terminated: true, encoding: ASCII)
      - id: compression_method
        type: u1
      - id: compressed_profile
        type: bytes

  trns_data:
    seq:
      - id: transparency
        type: bytes

  rgb:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1