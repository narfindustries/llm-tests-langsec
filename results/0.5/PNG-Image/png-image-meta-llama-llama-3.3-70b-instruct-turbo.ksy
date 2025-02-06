seq:
  - id: magic
    type: bytes
    size: 8
    contents: [137, 80, 78, 71, 13, 10, 26, 10]

  - id: ihdr
    type: chunk

  - id: chunks
    type: chunk
    repeat: eos

types:
  chunk:
    seq:
      - id: len
        type: u4le
      - id: type
        type: str
        size: 4
        enum:
          - IHDR
          - PLTE
          - IDAT
          - IEND
          - cHRM
          - gAMA
          - iCCP
          - sBIT
          - sRGB
          - tEXt
          - tIME
          - bKGD
          - pHYs
          - sPLT
          - hIST
          - acTL
          - fcTL
          - fdAT
      - id: data
        type:
          switch on: type
          cases:
            IHDR: ihdr_data
            PLTE: plte_data
            IDAT: idat_data
            IEND: iend_data
            cHRM: chrm_data
            gAMA: gama_data
            iCCP: iccp_data
            sBIT: sbit_data
            sRGB: srgb_data
            tEXt: text_data
            tIME: time_data
            bKGD: bkgd_data
            pHYs: phys_data
            sPLT: splte_data
            hIST: hist_data
            acTL: actl_data
            fcTL: fctl_data
            fdAT: fdAT_data
      - id: crc
        type: u4le

  ihdr_data:
    seq:
      - id: width
        type: u4le
      - id: height
        type: u4le
      - id: bit_depth
        type: u1
      - id: color_type
        type: u1
        enum:
          - 0
          - 2
          - 3
          - 4
          - 6
      - id: compression_method
        type: u1
        enum:
          - 0
      - id: filter_method
        type: u1
        enum:
          - 0
      - id: interlace_method
        type: u1
        enum:
          - 0
          - 1

  plte_data:
    seq:
      - id: palette
        type: palette_entry
        repeat: expr
        until: len == 0

  palette_entry:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

  idat_data:
    seq:
      - id: data
        type: bytes

  iend_data:
    seq: []

  chrm_data:
    seq:
      - id: white_point_x
        type: u4le
      - id: white_point_y
        type: u4le
      - id: red_x
        type: u4le
      - id: red_y
        type: u4le
      - id: green_x
        type: u4le
      - id: green_y
        type: u4le
      - id: blue_x
        type: u4le
      - id: blue_y
        type: u4le

  gama_data:
    seq:
      - id: gamma
        type: u4le

  iccp_data:
    seq:
      - id: profile_name
        type: str
        size: null-terminated
      - id: profile_data
        type: bytes

  sbit_data:
    seq:
      - id: significant_bits
        type: u1

  srgb_data:
    seq:
      - id: rendering_intent
        type: u1
        enum:
          - 0
          - 1
          - 2
          - 3

  text_data:
    seq:
      - id: keyword
        type: str
        size: null-terminated
      - id: text
        type: str
        size: null-terminated

  time_data:
    seq:
      - id: year
        type: u2le
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

  bkgd_data:
    seq:
      - id: bkgd
        type:
          switch on: ihdr.color_type
          cases:
            0: u1
            2: u2le
            3: u1
            4: u2le
            6: u4le

  phys_data:
    seq:
      - id: pixels_per_unit_x
        type: u4le
      - id: pixels_per_unit_y
        type: u4le
      - id: unit
        type: u1
        enum:
          - 0
          - 1

  splte_data:
    seq:
      - id: palette_name
        type: str
        size: null-terminated
      - id: palette_data
        type: bytes

  hist_data:
    seq:
      - id: histogram
        type: u2le
        repeat: expr
        until: len == 0

  actl_data:
    seq:
      - id: num_frames
        type: u4le
      - id: num_plays
        type: u4le

  fctl_data:
    seq:
      - id: sequence_number
        type: u4le
      - id: width
        type: u4le
      - id: height
        type: u4le
      - id: x_offset
        type: u4le
      - id: y_offset
        type: u4le
      - id: delay_num
        type: u2le
      - id: delay_den
        type: u2le
      - id: disposal_method
        type: u1
        enum:
          - 0
          - 1
          - 2
          - 3
      - id: blend_op
        type: u1
        enum:
          - 0
          - 1

  fdAT_data:
    seq:
      - id: sequence_number
        type: u4le
      - id: data
        type: bytes