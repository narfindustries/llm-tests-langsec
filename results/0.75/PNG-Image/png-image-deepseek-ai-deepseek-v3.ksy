meta:
  id: png
  file-extension: png
  endian: be
seq:
  - id: magic
    contents: "\x89PNG\r\n\x1a\n"
  - id: ihdr
    type: chunk_ihdr
  - id: chunks
    type: chunk
    repeat: until
    repeat-until: _.type == "IEND"
types:
  chunk:
    seq:
      - id: length
        type: u4
      - id: type
        type: str
        encoding: ASCII
        size: 4
      - id: data
        size: length
        type:
          switch-on: type
          cases:
            "IHDR": chunk_ihdr
            "PLTE": chunk_plte
            "IDAT": chunk_idat
            "IEND": chunk_iend
            "tRNS": chunk_trns
            "cHRM": chunk_chrm
            "gAMA": chunk_gama
            "iCCP": chunk_iccp
            "sBIT": chunk_sbit
            "sRGB": chunk_srgb
            "tEXt": chunk_text
            "zTXt": chunk_ztxt
            "iTXt": chunk_itxt
            "bKGD": chunk_bkgd
            "hIST": chunk_hist
            "pHYs": chunk_phys
            "sPLT": chunk_splt
            "tIME": chunk_time
      - id: crc
        type: u4
  chunk_ihdr:
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
  chunk_plte:
    seq:
      - id: entries
        type: rgb
        repeat: eos
  chunk_idat:
    seq:
      - id: data
        size: length
  chunk_iend:
    seq: []
  chunk_trns:
    seq:
      - id: transparency
        type:
          switch-on: _root.ihdr.color_type
          cases:
            0: trns_grayscale
            2: trns_rgb
            3: trns_palette
  trns_grayscale:
    seq:
      - id: gray
        type: u2
  trns_rgb:
    seq:
      - id: red
        type: u2
      - id: green
        type: u2
      - id: blue
        type: u2
  trns_palette:
    seq:
      - id: alpha
        type: u1
        repeat: eos
  chunk_chrm:
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
  chunk_gama:
    seq:
      - id: gamma
        type: u4
  chunk_iccp:
    seq:
      - id: profile_name
        type: str
        encoding: ASCII
        terminator: 0
      - id: compression_method
        type: u1
      - id: compressed_profile
        size: _parent.length - (_io.pos - _start)
  chunk_sbit:
    seq:
      - id: significant_bits
        type:
          switch-on: _root.ihdr.color_type
          cases:
            0: sbit_grayscale
            2: sbit_rgb
            3: sbit_palette
            4: sbit_grayscale_alpha
            6: sbit_rgba
  sbit_grayscale:
    seq:
      - id: gray
        type: u1
  sbit_rgb:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1
  sbit_palette:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1
  sbit_grayscale_alpha:
    seq:
      - id: gray
        type: u1
      - id: alpha
        type: u1
  sbit_rgba:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1
      - id: alpha
        type: u1
  chunk_srgb:
    seq:
      - id: rendering_intent
        type: u1
  chunk_text:
    seq:
      - id: keyword
        type: str
        encoding: ASCII
        terminator: 0
      - id: text
        type: str
        encoding: ISO-8859-1
        size-eos: true
  chunk_ztxt:
    seq:
      - id: keyword
        type: str
        encoding: ASCII
        terminator: 0
      - id: compression_method
        type: u1
      - id: compressed_text
        size: _parent.length - (_io.pos - _start)
  chunk_itxt:
    seq:
      - id: keyword
        type: str
        encoding: ASCII
        terminator: 0
      - id: compression_flag
        type: u1
      - id: compression_method
        type: u1
      - id: language_tag
        type: str
        encoding: ASCII
        terminator: 0
      - id: translated_keyword
        type: str
        encoding: UTF-8
        terminator: 0
      - id: text
        type: str
        encoding: UTF-8
        size-eos: true
  chunk_bkgd:
    seq:
      - id: background
        type:
          switch-on: _root.ihdr.color_type
          cases:
            0: bkgd_grayscale
            2: bkgd_rgb
            3: bkgd_palette
  bkgd_grayscale:
    seq:
      - id: gray
        type: u2
  bkgd_rgb:
    seq:
      - id: red
        type: u2
      - id: green
        type: u2
      - id: blue
        type: u2
  bkgd_palette:
    seq:
      - id: index
        type: u1
  chunk_hist:
    seq:
      - id: frequencies
        type: u2
        repeat: eos
  chunk_phys:
    seq:
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit_specifier
        type: u1
  chunk_splt:
    seq:
      - id: palette_name
        type: str
        encoding: ASCII
        terminator: 0
      - id: sample_depth
        type: u1
      - id: entries
        type: splt_entry
        repeat: eos
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
  chunk_time:
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
  rgb:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1