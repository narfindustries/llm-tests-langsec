meta:
  id: png
  title: Portable Network Graphics
  file-extension: png
  xref:
    iso: ISO/IEC 15948:2004
  license: CC0-1.0
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
      - id: len_data
        type: u4
      - id: type
        type: str
        size: 4
        encoding: ASCII
      - id: data
        size: len_data
        if: len_data > 0
      - id: crc
        type: u4

    instances:
      data_parsed:
        value: 'data_as(type)'
        if: len_data > 0 and type in ['IHDR', 'PLTE', 'IDAT', 'IEND', 'tRNS', 'gAMA', 'cHRM', 'sRGB', 'iCCP', 'tEXt', 'zTXt', 'iTXt', 'bKGD', 'pHYs', 'hIST', 'sBIT', 'sPLT', 'tIME']

  ihdr:
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

  plte:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: _parent.len_data / 3

  rgb:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

  idat:
    seq:
      - id: compressed_data
        size: _parent.len_data

  iend:
    seq: []

  trns:
    seq:
      - id: transparency_data
        size: _parent.len_data

  gama:
    seq:
      - id: gamma
        type: u4

  chrm:
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

  srgb:
    seq:
      - id: rendering_intent
        type: u1

  iccp:
    seq:
      - id: profile_name
        type: strz
        encoding: ASCII
      - id: compression_method
        type: u1
      - id: compressed_profile
        size: _parent.len_data - (profile_name.size + 2)

  text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
      - id: text
        size: _parent.len_data - keyword.size - 1

  ztxt:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
      - id: compression_method
        type: u1
      - id: compressed_text
        size: _parent.len_data - keyword.size - 2

  itxt:
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
        size: _parent.len_data - (keyword.size + 1 + 1 + language_tag.size + 1 + translated_keyword.size + 1)

  bkgd:
    seq:
      - id: background_color
        size: _parent.len_data

  phys:
    seq:
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit_specifier
        type: u1

  hist:
    seq:
      - id: frequency
        type: u2
        repeat: expr
        repeat-expr: _parent.len_data / 2

  sbit:
    seq:
      - id: significant_bits
        size: _parent.len_data

  splt:
    seq:
      - id: palette_name
        type: strz
        encoding: ASCII
      - id: sample_depth
        type: u1
      - id: entries
        type: splt_entry
        repeat: expr
        repeat-expr: (_parent.len_data - palette_name.size - 2) / (sample_depth == 8 ? 6 : 10)

  splt_entry:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1
      - id: alpha
        type: u1
      - id: frequency
        type: u2

  time:
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