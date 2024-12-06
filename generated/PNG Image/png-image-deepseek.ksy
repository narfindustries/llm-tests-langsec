generated/PNG Image/png-image-deepseek.ksy:
  meta:
    id: png_image_deepseek
    endian: le
  seq:
    - id: signature
      type: u4
      contents: [0x89, 0x50, 0x4E, 0x47]
    - id: ihdr
      type: ihdr_chunk
  types:
    ihdr_chunk:
      seq:
        - id: length
          type: u4
        - id: type
          type: u4
          contents: [0x49, 0x48, 0x44, 0x52]
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
        - id: crc
          type: u4