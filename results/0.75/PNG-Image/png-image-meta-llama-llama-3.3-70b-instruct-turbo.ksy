seq:
  - id: magic
    contents: [137, 80, 78, 71, 13, 10, 26, 10]
  - id: ihdr
    type: chunk
    doc: Image Header
  - id: chunks
    type: chunk
    repeat: eos
    eos-error: false
  - id: iend
    type: chunk
    doc: Image End

types:
  chunk:
    seq:
      - id: length
        type: u4le
      - id: chunk_type
        type: str
        size: 4
        encoding: ascii
      - id: data
        type: switch
        cases:
          - {on: chunk_type, equals: "IHDR", then: ihdr_data}
          - {on: chunk_type, equals: "PLTE", then: plte_data}
          - {on: chunk_type, equals: "IDAT", then: idat_data}
          - {on: chunk_type, equals: "IEND", then: iend_data}
          - {on: chunk_type, equals: "cHRM", then: chrm_data}
          - {on: chunk_type, equals: "gAMA", then: gama_data}
          - {on: chunk_type, equals: "sBIT", then: sbit_data}
          - {on: chunk_type, equals: "sRGB", then: srgb_data}
          - {on: chunk_type, equals: "iCCP", then: iccp_data}
          - {on: chunk_type, equals: "tRNS", then: trns_data}
          - {on: chunk_type, equals: "iTXt", then: itxt_data}
          - {on: chunk_type, equals: "tEXt", then: ttxt_data}
          - {on: chunk_type, equals: "zTXt", then: ztxt_data}
          - {on: chunk_type, equals: "bKGD", then: bkgd_data}
          - {on: chunk_type, equals: "hIST", then: hist_data}
          - {on: chunk_type, equals: "pCAL", then: pcal_data}
          - {on: chunk_type, equals: "sCAL", then: scal_data}
          - {on: chunk_type, equals: "tIME", then: time_data}
      - id: crc
        type: u4le
    doc: Chunk

  ihdr_data:
    seq:
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
    doc: Image Header Data

  plte_data:
    seq:
      - id: palette_entries
        type: plte_entry
        repeat: expr
        eos-error: false
        size: length

  plte_entry:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1
    doc: Palette Entry

  idat_data:
    seq:
      - id: compressed_image_data
        type: u1
        repeat: expr
        eos-error: false
        size: length
    doc: Image Data

  iend_data:
    seq: []
    doc: Image End Data

  chrm_data:
    seq:
      - id: white_point_x
        type: u4le
      - id: white_point_y
        type: u4le
      - id: red_x
        type: u4le
      - id: red_y
        type: u4le
      - id: green_x
        type: u4le
      - id: green_y
        type: u4le
      - id: blue_x
        type: u4le
      - id: blue_y
        type: u4le
    doc: Color Space Data

  gama_data:
    seq:
      - id: gamma
        type: u4le
    doc: Gamma Data

  sbit_data:
    seq:
      - id: significant_bits
        type: u1
    doc: Significant Bits Data

  srgb_data:
    seq:
      - id: rendering_intent
        type: u1
    doc: Standard RGB Color Space Data

  iccp_data:
    seq:
      - id: profile_name
        type: str
        size: length - 2
        encoding: ascii
      - id: compression_method
        type: u1
      - id: compressed_profile
        type: u1
        repeat: expr
        eos-error: false
        size: length - 2 - profile_name.length
    doc: ICC Profile Data

  trns_data:
    seq:
      - id: transparency_data
        type: u1
        repeat: expr
        eos-error: false
        size: length
    doc: Transparency Data

  itxt_data:
    seq:
      - id: keyword
        type: str
        size: 1
        encoding: ascii
      - id: null_byte
        contents: [0]
      - id: compression_flag
        type: u1
      - id: compression_method
        type: u1
      - id: language_tag
        type: str
        size: 0
        encoding: ascii
        repeat: expr
        eos-error: false
        until: _index >= length - 5 - keyword.length - 1
      - id: translated_keyword
        type: str
        size: 0
        encoding: ascii
        repeat: expr
        eos-error: false
        until: _index >= length - 5 - keyword.length - language_tag.length
      - id: text
        type: str
        size: 0
        encoding: ascii
        repeat: expr
        eos-error: false
        until: _index >= length - 5 - keyword.length - language_tag.length - translated_keyword.length
    doc: International Text Data

  ttxt_data:
    seq:
      - id: keyword
        type: str
        size: 1
        encoding: ascii
      - id: null_byte
        contents: [0]
      - id: text
        type: str
        size: 0
        encoding: ascii
        repeat: expr
        eos-error: false
        until: _index >= length - 1 - keyword.length
    doc: Text Data

  ztxt_data:
    seq:
      - id: keyword
        type: str
        size: 1
        encoding: ascii
      - id: null_byte
        contents: [0]
      - id: compression_method
        type: u1
      - id: compressed_text
        type: u1
        repeat: expr
        eos-error: false
        size: length - 2 - keyword.length
    doc: Compressed Text Data

  bkgd_data:
    seq:
      - id: background_color
        type: u1
        repeat: expr
        eos-error: false
        size: length
    doc: Background Data

  hist_data:
    seq:
      - id: histogram_data
        type: u1
        repeat: expr
        eos-error: false
        size: length
    doc: Histogram Data

  pcal_data:
    seq:
      - id: purpose
        type: u1
      - id: x0
        type: u4le
      - id: x1
        type: u4le
      - id: unit
        type: str
        size: 0
        encoding: ascii
        repeat: expr
        eos-error: false
        until: _index >= length - 10
      - id: type
        type: u1
      - id: data
        type: str
        size: 0
        encoding: ascii
        repeat: expr
        eos-error: false
        until: _index >= length - 10 - unit.length
    doc: Physical Scale Data

  scal_data:
    seq:
      - id: unit
        type: str
        size: 0
        encoding: ascii
        repeat: expr
        eos-error: false
        until: _index >= length - 12
      - id: width
        type: u4le
      - id: height
        type: u4le
      - id: scale
        type: u4le
    doc: Scale Data

  time_data:
    seq:
      - id: year
        type: u2le
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
    doc: Last Modification Time Data