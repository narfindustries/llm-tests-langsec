meta:
  id: png
  file-extension: png
  endian: be

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
            "'IHDR'": ihdr_chunk
            "'PLTE'": plte_chunk
            "'tRNS'": trns_chunk
            "'tEXt'": text_chunk
            "'zTXt'": compressed_text_chunk
            "'iTXt'": international_text_chunk
            "'bKGD'": background_chunk
            "'pHYs'": physical_dimension_chunk
            "'tIME'": time_chunk
            "'gAMA'": gamma_chunk
            "'cHRM'": chromaticity_chunk
            "'sRGB'": srgb_chunk
            "'iCCP'": icc_profile_chunk
            "'IDAT'": image_data_chunk
            "'IEND'": end_chunk
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
        enum: bit_depth_values
      - id: color_type
        type: u1
        enum: color_type_values
      - id: compression_method
        type: u1
        enum: compression_method_values
      - id: filter_method
        type: u1
        enum: filter_method_values
      - id: interlace_method
        type: u1
        enum: interlace_method_values

  plte_chunk:
    seq:
      - id: palette_entries
        type: palette_entry
        repeat: expr
        repeat-expr: _parent.length / 3

  palette_entry:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

  trns_chunk:
    seq:
      - id: transparency_data
        type: u1
        repeat: eos

  text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ISO-8859-1
      - id: text
        type: strz
        encoding: ISO-8859-1

  compressed_text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ISO-8859-1
      - id: compression_method
        type: u1
      - id: compressed_text
        type: str
        size-eos: true
        encoding: ISO-8859-1

  international_text_chunk:
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

  background_chunk:
    seq:
      - id: background_color
        type: u1
        repeat: eos

  physical_dimension_chunk:
    seq:
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit
        type: u1
        enum: unit_specifier

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

  gamma_chunk:
    seq:
      - id: gamma_value
        type: u4

  chromaticity_chunk:
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
        enum: srgb_rendering_intent

  icc_profile_chunk:
    seq:
      - id: profile_name
        type: strz
        encoding: ASCII
      - id: compression_method
        type: u1
      - id: compressed_profile
        type: str
        size-eos: true
        encoding: ASCII

  image_data_chunk:
    seq:
      - id: image_data
        type: str
        size-eos: true
        encoding: ASCII

  end_chunk:
    seq: []

enums:
  bit_depth_values:
    1: one_bit
    2: two_bit
    4: four_bit
    8: eight_bit
    16: sixteen_bit

  color_type_values:
    0: grayscale
    2: rgb
    3: palette
    4: grayscale_alpha
    6: rgb_alpha

  compression_method_values:
    0: deflate_inflate

  filter_method_values:
    0: adaptive_filtering

  interlace_method_values:
    0: no_interlace
    1: adam7_interlace

  unit_specifier:
    0: unknown
    1: meter

  srgb_rendering_intent:
    0: perceptual
    1: relative_colorimetric
    2: saturation
    3: absolute_colorimetric