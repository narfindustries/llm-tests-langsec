meta:
  id: png
  endian: be

seq:
  - id: magic
    type: bytes
    size: 8
    contents: [137, 80, 78, 71, 13, 10, 26, 10]

  - id: chunks
    type: chunk
    repeat: until

types:
  chunk:
    seq:
      - id: len
        type: u4
      - id: type
        type: str
        size: 4
      - id: data
        type:
          switch-on: type
          cases:
            'IHDR': ihdr
            'PLTE': plte
            'IDAT': idat
            'IEND': iend
            'tEXt': text
            'tIME': time
            'zTXt': ztxt
            else: bytes
        size: len
      - id: crc
        type: u4

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
      - id: palette_entries
        type: palette_entry
        repeat: expr
        repeat_expr: len / 3

  idat:
    seq:
      - id: compressed_image_data
        type: bytes
        size: len

  iend:
    seq:
      - id: crc
        type: u4

  palette_entry:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

  text:
    seq:
      - id: keyword
        type: str
        size: null
        term: 0
      - id: text_data
        type: str
        size: null
        term: 0

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

  ztxt:
    seq:
      - id: keyword
        type: str
        size: null
        term: 0
      - id: compression_method
        type: u1
      - id: compressed_text_data
        type: bytes
        size: null

  enums:
    color_type:
      0: grayscale
      2: truecolor
      3: palette
      4: grayscale_alpha
      6: truecolor_alpha
    compression_method:
      0: deflate
    filter_method:
      0: adaptive
    interlace_method:
      0: none
      1: adam7
    bit_depth:
      1: bit1
      2: bit2
      4: bit4
      8: bit8
      16: bit16