meta:
  id: png
  title: PNG Image
  file-extension: png
  xref:
    mime: image/png

seq:
  - id: signature
    contents: [137, 80, 78, 71, 13, 10, 26, 10]

  - id: chunks
    type: chunk
    repeat: until
    repeat-until: _.type == "IEND"

types:
  chunk:
    seq:
      - id: length
        type: u4be

      - id: type
        type: str
        size: 4
        encoding: ASCII

      - id: data
        size: length
        type:
          switch-on: type
          cases:
            '"IHDR"': ihdr
            '"PLTE"': plte
            '"IDAT"': idat
            '"IEND"': iend
            '"tEXt"': text
            '"zTXt"': ztxt
            '"iTXt"': itxt
            '"bKGD"': bkgd
            '"cHRM"': chrm
            '"gAMA"': gama
            '"sBIT"': sbit
            '"sRGB"': srgb
            '"tIME"': time
            '"tRNS"': trns

      - id: crc
        type: u4be

  ihdr:
    seq:
      - id: width
        type: u4be
      - id: height
        type: u4be
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
        type: plte_entry
        repeat: eos

  plte_entry:
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
        size: _parent.length

  iend:
    seq: []

  text:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
      - id: text
        size: _parent.length - keyword.size - 1
        type: str
        encoding: ISO-8859-1

  ztxt:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
      - id: compression_method
        type: u1
      - id: compressed_text
        size: _parent.length - keyword.size - 2

  itxt:
    seq:
      - id: compressed
        type: u1
      - id: compression_flag
        type: u1
      - id: language_tag
        type: strz
        encoding: UTF-8
      - id: translated_keyword
        type: strz
        encoding: UTF-8
      - id: text
        size: _parent.length - 3 - keyword.size - language_tag.size - 1 - translated_keyword.size - 1
        type: str
        encoding: UTF-8

  bkgd:
    seq:
      - id: background
        size: _parent.length

  chrm:
    seq:
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

  gama:
    seq:
      - id: gamma
        type: u4be

  sbit:
    seq:
      - id: bits
        size: _parent.length

  srgb:
    seq:
      - id: rendering_intent
        type: u1

  time:
    seq:
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

  trns:
    seq:
      - id: transparency_data
        size: _parent.length