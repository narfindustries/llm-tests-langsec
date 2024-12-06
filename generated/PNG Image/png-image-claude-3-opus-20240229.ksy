meta:
  id: png_image_claude_3_opus_20240229
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
  - id: idat_chunks
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
      - id: compression_method
        type: u1
      - id: filter_method
        type: u1
      - id: interlace_method
        type: u1
      - id: crc
        type: u4
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
      - id: crc
        type: u4