meta:
  id: png
  title: Portable Network Graphics
  file-extension: png
  xref:
    iso: ISO/IEC 15948:2004
  endian: be

seq:
  - id: signature
    contents: [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
  - id: chunks
    type: chunk
    repeat: until
    repeat-until: _.type == "IEND"

types:
  chunk:
    seq:
      - id: len
        type: u4
      - id: type
        type: str
        size: 4
        encoding: ASCII
      - id: body
        size: len
        type:
          switch-on: type
          cases:
            'IHDR': ihdr_chunk
            'PLTE': plte_chunk
            'IDAT': idat_chunk
            'IEND': iend_chunk
            'tEXt': text_chunk
            'zTXt': ztxt_chunk
            'iTXt': itxt_chunk
            'bKGD': bkgd_chunk
            'cHRM': chrm_chunk
            'gAMA': gama_chunk
            'hIST': hist_chunk
            'pHYs': phys_chunk
            'sBIT': sbit_chunk
            'sPLT': splt_chunk
            'sRGB': srgb_chunk
            'tIME': time_chunk
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
      - id: entries
        type: rgb
        repeat: eos

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
      - id: data
        size: _parent.len

  iend_chunk:
    seq: []

  text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
      - id: text
        type: str
        size: _parent.len - keyword.length - 1
        encoding: ISO-8859-1

  ztxt_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
      - id: compression_method
        type: u1
      - id: compressed_text
        size: _parent.len - keyword.length - 2
        process: zlib

  itxt_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: ASCII
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
        type: str
        size: _parent.len - keyword.length - 5 - language_tag.length - translated_keyword.length
        encoding: UTF-8
        process: if compression_flag == 1 then zlib else null

  bkgd_chunk:
    seq:
      - id: palette_index
        type: u1
        if: _root.chunks[0].body.color_type == 3
      - id: gray
        type: u2
        if: _root.chunks[0].body.color_type == 0 || _root.chunks[0].body.color_type == 4
      - id: red
        type: u2
        if: _root.chunks[0].body.color_type == 2 || _root.chunks[0].body.color_type == 6
      - id: green
        type: u2
        if: _root.chunks[0].body.color_type == 2 || _root.chunks[0].body.color_type == 6
      - id: blue
        type: u2
        if: _root.chunks[0].body.color_type == 2 || _root.chunks[0].body.color_type == 6

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

  hist_chunk:
    seq:
      - id: frequency
        type: u2
        repeat: eos

  phys_chunk:
    seq:
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit_specifier
        type: u1

  sbit_chunk:
    seq:
      - id: grayscale_bits
        type: u1
        if: _root.chunks[0].body.color_type == 0 || _root.chunks[0].body.color_type == 4
      - id: red_bits
        type: u1
        if: _root.chunks[0].body.color_type == 2 || _root.chunks[0].body.color_type == 3 || _root.chunks[0].body.color_type == 6
      - id: green_bits
        type: u1
        if: _root.chunks[0].body.color_type == 2 || _root.chunks[0].body.color_type == 3 || _root.chunks[0].body.color_type == 6
      - id: blue_bits
        type: u1
        if: _root.chunks[0].body.color_type == 2 || _root.chunks[0].body.color_type == 3 || _root.chunks[0].body.color_type == 6
      - id: alpha_bits
        type: u1
        if: _root.chunks[0].body.color_type == 4 || _root.chunks[0].body.color_type == 6

  splt_chunk:
    seq:
      - id: name
        type: strz
        encoding: ASCII
      - id: sample_depth
        type: u1
      - id: entries
        type: splt_entry
        repeat: eos

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

  srgb_chunk:
    seq:
      - id: rendering_intent
        type: u1

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