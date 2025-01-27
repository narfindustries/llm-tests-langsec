meta:
  id: png-image-gemini-1
  title: PNG Image (Gemini 1)
  homepage: https://kaitai.io/
  file_extension: png
  experimental: true

types:
  png_image_header:
    seq:
      - id: signature
        type: u4
        enum: png_signature
      - id: ihdr
        type: png_ihdr
      - id: idat
        type: seq
        repeat: eos
        read_until: lambda x: x.len == 0
        contents:
          - id: chunk_type
            type: u4
            enum: png_chunk_type
          - id: chunk_data_length
            type: u4
          - id: chunk_data
            type: u4
            repeat: expr
            repeat-expr: chunk_data_length
          - id: crc
            type: u4

  png_ihdr:
    seq:
      - id: width
        type: u4
      - id: height
        type: u4
      - id: bit_depth
        type: u1
      - id: color_type
        type: u1
        enum: png_color_type
      - id: compression_method
        type: u1
      - id: filter_method
        type: u1
      - id: interlace_method
        type: u1

enums:
  png_signature:
    0x89504e47: png_signature_png
  png_chunk_type:
    0x49484452: png_chunk_type_ihdr
    0x49444154: png_chunk_type_idat
    0x49454e44: png_chunk_type_iend
  png_color_type:
    0: png_color_type_grayscale
    2: png_color_type_truecolor
    3: png_color_type_indexed
    4: png_color_type_grayscale_alpha
    6: png_color_type_truecolor_alpha

seq:
  - id: header
    type: png_image_header

