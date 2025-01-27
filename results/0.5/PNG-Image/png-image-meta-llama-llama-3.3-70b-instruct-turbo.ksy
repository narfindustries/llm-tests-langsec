meta:
  id: png_image
  title: PNG Image
  file-extension: png
  endian: le
seq:
  - id: magic
    type: str
    size: 8
    enum: ["\x89PNG\r\n\x1a\n"]
  - id: ihdr
    type: chunk
  - id: chunks
    type: chunk
    repeat: until
    repeat-until: type == "\x49\x45\x4e\x44"
  - id: iend
    type: chunk
    size: 0
types:
  chunk:
    seq:
      - id: length
        type: u4
      - id: type
        type: str
        size: 4
      - id: data
        type: switch-on type
        cases:
          "\x49\x48\x44\x52": seq:
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
          "\x49\x44\x41\x54": seq:
            - id: data
              type: bytes
              size: length
          "\x49\x45\x4e\x44": seq:
            - id: data
              type: bytes
              size: 0
      - id: crc
        type: u4