meta:
  id: png
  file-extension: png
  endian: be
  license: CC0-1.0
  title: Portable Network Graphics (PNG)
seq:
  - id: signature
    contents: [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
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
            '"tEXt"': text_chunk
            '"sRGB"': srgb_chunk
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
        enum: color_type
      - id: compression_method
        type: u1
      - id: filter_method
        type: u1
      - id: interlace_method
        type: u1
        enum: interlace_method
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
      - id: data_bytes
        size: _parent.length
  iend_chunk:
    seq: # Empty sequence since no fields are expected
  trns_chunk:
    seq:
      - id: alpha_values
        size: _parent.length
        type: u1
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
        type: u1
  text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: UTF-8
      - id: text
        type: str
        size-eos: true
        encoding: UTF-8
  srgb_chunk:
    seq:
      - id: rendering_intent
        type: u1
enums:
  color_type:
    0: grayscale
    2: truecolor
    3: indexed_color
    4: grayscale_alpha
    6: truecolor_alpha
  interlace_method:
    0: no_interlace
    1: adam7