meta:
  id: png_image
  file-extension: png
  endian: be

seq:
  - id: magic
    contents: [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]
  - id: chunks
    type: chunk
    repeat: until
    repeat-until: _.type == 'IEND'

types:
  chunk:
    seq:
      - id: length
        type: u4
      - id: type
        type: str
        size: 4
        encoding: ascii
      - id: data
        size: length
      - id: crc
        type: u4

    instances:
      is_critical:
        value: (type.to_s.substring(0, 1).upcase == type.to_s.substring(0, 1))

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

    enums:
      color_types:
        0: grayscale
        2: rgb
        3: palette
        4: grayscale_alpha
        6: rgb_alpha

      interlace_methods:
        0: none
        1: adam7