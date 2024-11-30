meta:
  id: png
  file-extension: png
  endian: be

seq:
  - id: magic
    contents: [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

  - id: ihdr_len
    type: u4

  - id: ihdr_type
    contents: "IHDR"
    
  - id: ihdr
    type: ihdr_chunk
    
  - id: ihdr_crc
    size: 4

  - id: chunks
    type: chunk
    repeat: until
    repeat-until: _.type == "IEND" or _io.eof

types:
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
        enum: color_type
      - id: compression_method
        type: u1
      - id: filter_method
        type: u1
      - id: interlace_method
        type: u1

  chunk:
    seq:
      - id: len
        type: u4
      - id: type
        type: str
        size: 4
        encoding: UTF-8
      - id: body
        size: len
        type:
          switch-on: type
          cases:
            '"PLTE"': plte_chunk
            '"tRNS"': trns_chunk
            '"gAMA"': gama_chunk
            '"cHRM"': chrm_chunk
            '"sRGB"': srgb_chunk
            '"pHYs"': phys_chunk
            '"iTXt"': international_text_chunk
            '"tEXt"': text_chunk
            '"zTXt"': compressed_text_chunk
            '"bKGD"': bkgd_chunk
      - id: crc
        size: 4
        
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

  trns_chunk:
    seq:
      - id: data
        size-eos: true

  gama_chunk:
    seq:
      - id: gamma_int
        type: u4

  chrm_chunk:
    seq:
      - id: white_point
        type: point
      - id: red
        type: point
      - id: green
        type: point
      - id: blue
        type: point
        
  point:
    seq:
      - id: x_int
        type: u4
      - id: y_int
        type: u4
        
  srgb_chunk:
    seq:
      - id: render_intent
        type: u1
        
  phys_chunk:
    seq:
      - id: pixels_per_unit_x
        type: u4
      - id: pixels_per_unit_y
        type: u4
      - id: unit
        type: u1
        
  text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: iso8859-1
      - id: text
        size-eos: true
        
  compressed_text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: iso8859-1
      - id: compression_method
        type: u1
      - id: text_datastream
        size-eos: true
        process: zlib

  international_text_chunk:
    seq:
      - id: keyword
        type: strz
        encoding: UTF-8
      - id: compression_flag
        type: u1
      - id: compression_method
        type: u1