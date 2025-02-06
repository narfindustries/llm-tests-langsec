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
    repeat-until: _.type == 'IEND'

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
        enum: bit_depths
      - id: color_type
        type: u1
        enum: color_types
      - id: compression_method
        type: u1
        enum: compression_methods
      - id: filter_method
        type: u1
        enum: filter_methods
      - id: interlace_method
        type: u1
        enum: interlace_methods

  plte_chunk:
    seq:
      - id: palette_entries
        type: rgb
        repeat: expr
        repeat-expr: _parent.length / 3

  trns_chunk:
    seq:
      - id: alpha_values
        type: u1
        repeat: eos

  gama_chunk:
    seq:
      - id: gamma_value
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

  iccp_chunk:
    seq:
      - id: profile_name
        type: strz
        encoding: ASCII
      - id: compression_method
        type: u1
      - id: compressed_profile
        type: byte_array

  text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
      - id: text
        type: strz
        encoding: UTF-8

  ztxt_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
      - id: compression_method
        type: u1
      - id: compressed_text
        type: byte_array

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
    params:
      - id: color_type
        type: u1
        enum: color_types
    seq:
      - id: background
        type:
          switch-on: color_type
          cases:
            'color_types::grayscale': u2
            'color_types::rgb': rgb
            'color_types::palette_index': u1

  phys_chunk:
    seq:
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit
        type: u1
        enum: unit_types

  sbit_chunk:
    params:
      - id: color_type
        type: u1
        enum: color_types
    seq:
      - id: significant_bits
        type:
          switch-on: color_type
          cases:
            'color_types::grayscale': grayscale_sbit
            'color_types::rgb': rgb_sbit
            'color_types::palette_index': palette_sbit
            'color_types::grayscale_alpha': grayscale_alpha_sbit
            'color_types::rgb_alpha': rgba_sbit

  splt_chunk:
    seq:
      - id: palette_name
        type: strz
        encoding: ASCII
      - id: sample_depth
        type: u1
      - id: palette_entries
        type: splt_entry
        repeat: eos

  hist_chunk:
    seq:
      - id: frequency
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
        type: byte_array

  byte_array:
    seq:
      - id: data
        type: u1
        repeat: eos

  rgb:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

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

  grayscale_sbit:
    seq:
      - id: gray_bits
        type: u1

  rgb_sbit:
    seq:
      - id: red_bits
        type: u1
      - id: green_bits
        type: u1
      - id: blue_bits
        type: u1

  palette_sbit:
    seq:
      - id: red_bits
        type: u1
      - id: green_bits
        type: u1
      - id: blue_bits
        type: u1

  grayscale_alpha_sbit:
    seq:
      - id: gray_bits
        type: u1
      - id: alpha_bits
        type: u1

  rgba_sbit:
    seq:
      - id: red_bits
        type: u1
      - id: green_bits
        type: u1
      - id: blue_bits
        type: u1
      - id: alpha_bits
        type: u1

enums:
  bit_depths:
    1: one_bit
    2: two_bits
    4: four_bits
    8: eight_bits
    16: sixteen_bits

  color_types:
    0: grayscale
    2: rgb
    3: palette_index
    4: grayscale_alpha
    6: rgb_alpha

  compression_methods:
    0: deflate_inflate

  filter_methods:
    0: adaptive_filtering

  interlace_methods:
    0: no_interlace
    1: adam7_interlace

  rendering_intents:
    0: perceptual
    1: relative_colorimetric
    2: saturation
    3: absolute_colorimetric

  unit_types:
    0: unknown
    1: meter