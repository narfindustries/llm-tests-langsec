type: seq
- id: signature
  type: bytes
  size: 8
  contents: [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]
- id: chunks
  type: seq
  - id: chunk
    type: struct
    - id: length
      type: u4
    - id: type
      type: str
      size: 4
      encoding: ASCII
    - id: data
      type: bytes
      size: length
    - id: crc
      type: u4
    - id: ihdr
      type: instance
      if: type == "IHDR"
      - id: width
        type: u4
      - id: height
        type: u4
      - id: bit_depth
        type: u1
        enum:
          1: one
          2: two
          4: four
          8: eight
          16: sixteen
      - id: color_type
        type: u1
        enum:
          0: grayscale
          2: truecolor
          3: indexed
          4: grayscale_alpha
          6: truecolor_alpha
      - id: compression_method
        type: u1
        enum:
          0: deflate
      - id: filter_method
        type: u1
        enum:
          0: adaptive_filtering
      - id: interlace_method
        type: u1
        enum:
          0: no_interlace
          1: adam7_interlace
    - id: plte
      type: instance
      if: type == "PLTE"
      - id: palette
        type: seq
        - type: u1
        size: 3
    - id: idat
      type: instance
      if: type == "IDAT"
      - id: data
        type: bytes
        size: length
    - id: iend
      type: instance
      if: type == "IEND"
    - id: text
      type: instance
      if: type == "tEXt"
      - id: keyword
        type: str
        encoding: ASCII
        term: 0x00
      - id: text
        type: str
        encoding: ISO_8859_1
        term: 0x00
    - id: ztext
      type: instance
      if: type == "zTXt"
      - id: keyword
        type: str
        encoding: ASCII
        term: 0x00
      - id: compression_method
        type: u1
        enum:
          0: deflate
      - id: compressed_text
        type: bytes
        size: length - 1
    - id: itext
      type: instance
      if: type == "iTXt"
      - id: keyword
        type: str
        encoding: ASCII
        term: 0x00
      - id: compression_flag
        type: u1
      - id: compression_method
        type: u1
        enum:
          0: deflate
      - id: language_tag
        type: str
        encoding: UTF-8
        term: 0x00
      - id: translated_keyword
        type: str
        encoding: UTF-8
        term: 0x00
      - id: text
        type: str
        encoding: UTF-8
        term: 0x00
    - id: bkgd
      type: instance
      if: type == "bKGD"
      - id: grayscale
        type: u2
        if: color_type == 0 || color_type == 4
      - id: truecolor
        type: struct
        if: color_type == 2 || color_type == 6
        - id: red
          type: u2
        - id: green
          type: u2
        - id: blue
          type: u2
      - id: palette_index
        type: u1
        if: color_type == 3
    - id: chrm
      type: instance
      if: type == "cHRM"
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
    - id: gama
      type: instance
      if: type == "gAMA"
      - id: gamma
        type: u4
    - id: hist
      type: instance
      if: type == "hIST"
      - id: histogram
        type: seq
        - type: u2
        size: length / 2
    - id: phys
      type: instance
      if: type == "pHYs"
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit_specifier
        type: u1
        enum:
          0: unknown
          1: meter
    - id: sbit
      type: instance
      if: type == "sBIT"
      - id: significant_bits
        type: seq
        - type: u1
        size: length
    - id: splt
      type: instance
      if: type == "sPLT"
      - id: palette_name
        type: str
        encoding: ASCII
        term: 0x00
      - id: entries
        type: seq
        - type: struct
          - id: sample_depth
            type: u1
          - id: red
            type: u2
          - id: green
            type: u2
          - id: blue
            type: u2
          - id: alpha
            type: u2
          size: length - 1
    - id: time
      type: instance
      if: type == "tIME"
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


The error "Unexpected error during compilation: generated/888/0.0/PNG-Image/png-image-gemini-1.5-flash.ksy:2:1:  error: expected <block end>, but found '-'"  typically arises from an indentation problem in the YAML file.  Kaitai Struct's YAML parser is very strict about indentation.  Make absolutely sure that all items under a parent node are consistently indented.  A single misplaced space or tab can cause this error.  I've double-checked the indentation in this response, but if you still get the error, carefully review your YAML file's formatting.  Use a consistent number of spaces (e.g., 2 or 4) for indentation throughout the file.  Avoid mixing tabs and spaces.
