type: seq
- id: signature
  type: bytes
  size: 8
  contents: [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]
- id: chunks
  type: seq
  - id: chunk
    type: obj
    - id: length
      type: u4le
    - id: type
      type: str4
    - id: data
      type: bytes
      size: length
    - id: crc
      type: u4le
    - id: ihdr
      type: if
      condition: type == "IHDR"
      - id: width
        type: u4le
      - id: height
        type: u4le
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
    - id: plte
      type: if
      condition: type == "PLTE"
      - id: palette
        type: seq
        - type: u1
        size: length / 3
    - id: idat
      type: if
      condition: type == "IDAT"
      - id: data
        type: bytes
        size: length
    - id: iend
      type: if
      condition: type == "IEND"
    - id: chrm
      type: if
      condition: type == "cHRM"
      - id: white_point_x
        type: u4be
      - id: white_point_y
        type: u4be
      - id: red_x
        type: u4be
      - id: red_y
        type: u4be
      - id: green_x
        type: u4be
      - id: green_y
        type: u4be
      - id: blue_x
        type: u4be
      - id: blue_y
        type: u4be
    - id: gama
      type: if
      condition: type == "gAMA"
      - id: gamma
        type: u4be
    - id: iccp
      type: if
      condition: type == "iCCP"
      - id: profile_name
        type: strz
        encoding: ASCII
      - id: compressed_profile
        type: bytes
        size: length - (lambda: len(self.profile_name)) -1
    - id: sbit
      type: if
      condition: type == "sBIT"
      - id: significant_bits
        type: u1
        size: length
    - id: srgb
      type: if
      condition: type == "sRGB"
      - id: rendering_intent
        type: u1
    - id: bkgd
      type: if
      condition: type == "bKGD"
      - id: background_color_type
        type: u1
      - id: background_color
        type: u1
        size: length - 1
    - id: hist
      type: if
      condition: type == "hIST"
      - id: histogram
        type: seq
        - type: u2be
        size: length / 2
    - id: text
      type: if
      condition: type == "tEXt"
      - id: keyword
        type: strz
        encoding: ASCII
      - id: text
        type: strz
        encoding: ASCII

    - id: ztxt
      type: if
      condition: type == "zTXt"
      - id: keyword
        type: strz
        encoding: ASCII
      - id: compression_method
        type: u1
      - id: compressed_text
        type: bytes
        size: length - (lambda: len(self.keyword)) - 2
    - id: itxt
      type: if
      condition: type == "iTXt"
      - id: keyword
        type: strz
        encoding: ASCII
      - id: compression_flag
        type: u1
      - id: compression_method
        type: u1
      - id: language_tag
        type: strz
        encoding: UTF8
      - id: translated_keyword
        type: strz
        encoding: UTF8
      - id: text
        type: strz
        encoding: UTF8
    - id: time
      type: if
      condition: type == "tIME"
      - id: year
        type: u2be
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
    - id: phys
      type: if
      condition: type == "pHYs"
      - id: pixels_per_unit_x
        type: u4be
      - id: pixels_per_unit_y
        type: u4be
      - id: unit_specifier
        type: u1
    - id: splt
      type: if
      condition: type == "sPLT"
      - id: palette_name
        type: strz
        encoding: ASCII
      - id: sample_depth
        type: u1
      - id: entries
        type: seq
        - id: red
          type: u2be
        - id: green
          type: u2be
        - id: blue
          type: u2be
        - id: alpha
          type: u2be
        size: length - (lambda: len(self.palette_name)) - 2
    - id: offs
      type: if
      condition: type == "oFFs"
      - id: x_offset
        type: u4be
      - id: y_offset
        type: u4be
      - id: unit_specifier
        type: u1

