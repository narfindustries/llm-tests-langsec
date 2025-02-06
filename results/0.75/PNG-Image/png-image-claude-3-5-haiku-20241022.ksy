meta:
  id: png
  file-extension: png
  endian: be

seq:
  - id: signature
    contents: [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
  
  - id: chunks
    type: chunk
    repeat: until
    repeat-until: _.chunk_type == "IEND"

types:
  chunk:
    seq:
      - id: length
        type: u4
      
      - id: chunk_type
        type: str
        size: 4
        encoding: ASCII
      
      - id: chunk_data
        size: length
        type:
          switch-on: chunk_type
          cases:
            '"IHDR"': header_chunk
            '"PLTE"': palette_chunk
            '"IDAT"': image_data_chunk
            '"IEND"': end_chunk
            '"tRNS"': transparency_chunk
            '"gAMA"': gamma_chunk
            '"cHRM"': chromaticity_chunk
            '"sRGB"': srgb_chunk
            '"tEXt"': text_chunk
            '"zTXt"': compressed_text_chunk
            '"iTXt"': international_text_chunk
            '"bKGD"': background_chunk
            '"pHYs"': physical_chunk
            '"tIME"': time_chunk
      
      - id: crc
        type: u4
    
    types:
      header_chunk:
        seq:
          - id: width
            type: u4
          - id: height
            type: u4
          - id: bit_depth
            type: u1
          - id: color_type
            type: u1
            enum: color_types
          - id: compression_method
            type: u1
          - id: filter_method
            type: u1
          - id: interlace_method
            type: u1
      
      palette_chunk:
        seq:
          - id: palette_entries
            type: rgb
            repeat: expr
            repeat-expr: _parent.length / 3
      
      rgb:
        seq:
          - id: red
            type: u1
          - id: green
            type: u1
          - id: blue
            type: u1
      
      image_data_chunk:
        seq:
          - id: image_data
            type: str
            size-eos: true
            encoding: ASCII
      
      end_chunk:
        seq: []
      
      transparency_chunk:
        seq:
          - id: transparency_data
            type: str
            size-eos: true
            encoding: ASCII
      
      gamma_chunk:
        seq:
          - id: gamma_value
            type: u4
      
      chromaticity_chunk:
        seq:
          - id: white_x
            type: u4
          - id: white_y
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
            enum: rendering_intents
      
      text_chunk:
        seq:
          - id: keyword
            type: strz
            encoding: ASCII
          - id: text
            type: strz
            encoding: ASCII
      
      compressed_text_chunk:
        seq:
          - id: keyword
            type: strz
            encoding: ASCII
          - id: compression_method
            type: u1
          - id: compressed_text
            type: str
            size-eos: true
            encoding: ASCII
      
      international_text_chunk:
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
            type: strz
            encoding: UTF-8
      
      background_chunk:
        seq:
          - id: background_data
            type: str
            size-eos: true
            encoding: ASCII
      
      physical_chunk:
        seq:
          - id: pixels_per_unit_x
            type: u4
          - id: pixels_per_unit_y
            type: u4
          - id: unit_specifier
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

enums:
  color_types:
    0: grayscale
    2: rgb
    3: palette
    4: grayscale_alpha
    6: rgb_alpha
  
  rendering_intents:
    0: perceptual
    1: relative_colorimetric
    2: saturation
    3: absolute_colorimetric