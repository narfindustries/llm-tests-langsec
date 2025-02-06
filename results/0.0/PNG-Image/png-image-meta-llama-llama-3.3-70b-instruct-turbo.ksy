meta:
  endian: be

types:
  chunk:
    seq:
      - id: length
        type: u4
      - id: chunk_type
        type: str
        size: 4
        encoding: ascii
      - id: chunk_data
        type: bytes
        size: length
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
      - id: palette_entries
        type: rgb_triple
        repeat: expr
        repeat-expr: length / 3

  rgb_triple:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

  idat_chunk:
    seq:
      - id: compressed_image_data
        type: bytes
        size: length

  iend_chunk:
    seq: []

  bkgd_chunk:
    seq:
      - id: bkgd_color
        type: rgb_triple

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
        type: str
        size: 79
        encoding: ascii
      - id: compression_method
        type: u1
      - id: compressed_profile
        type: bytes
        size: length - 80

  sbit_chunk:
    seq:
      - id: significant_bits
        type: u1
        repeat: expr
        repeat-expr: 3

  srgb_chunk:
    seq:
      - id: rendering_intent
        type: u1

  text_chunk:
    seq:
      - id: keyword
        type: str
        size: 79
        encoding: ascii
      - id: text_data
        type: bytes
        size: length - 80

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

  phys_chunk:
    seq:
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit_specifier
        type: u1

  splte_chunk:
    seq:
      - id: palette_name
        type: str
        size: 79
        encoding: ascii
      - id: palette_entries
        type: rgb_triple
        repeat: expr
        repeat-expr: (length - 80) / 10

  hist_chunk:
    seq:
      - id: histogram
        type: u2
        repeat: expr
        repeat-expr: 256

seq:
  - id: signature
    type: bytes
    size: 8
    contents: [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]
  - id: chunks
    type: chunk
    repeat: until
    repeat-until: chunk_type == "IEND"