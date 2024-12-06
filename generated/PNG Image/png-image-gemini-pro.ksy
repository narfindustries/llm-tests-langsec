- &png_header
  - type: u32le
    endian: le
    name: signature
- &chunk_header
  - type: u32le
    endian: le
    name: length
  - type: string
    size: 4
    encoding: ascii
    name: type
- &ihdr
  - extends: &chunk_header
  - type: u32le
    endian: le
    name: width
  - type: u32le
    endian: le
    name: height
  - type: u8
    name: bit_depth
  - type: u8
    name: color_type
  - type: u8
    name: compression_method
  - type: u8
    name: filter_method
  - type: u8
    name: interlace_method
- &idat
  - extends: &chunk_header
  - type: bytes
    name: data
- &iend
  - extends: &chunk_header
- &png
  - type: struct
    name: PNG
    fields:
      - &png_header
      - &ihdr
      - &idat
      - &iend  