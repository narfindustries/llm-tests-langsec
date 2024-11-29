meta:
  id: png
  endian: be
seq:
  - id: signature
    contents: [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
    doc: PNG signature
  - id: chunks
    type: chunk
    repeat: eos

types:
  chunk:
    seq:
      - id: length
        type: u4
        doc: Length of the chunk data
      - id: type
        type: str
        size: 4
        encoding: ASCII
        doc: Type of the chunk
      - id: data
        type: u1
        repeat: expr
        repeat-expr: length
      - id: crc
        type: u4
        doc: CRC-32 checksum of the chunk type and data

  ihdr_chunk:
    seq:
      - id: width
        type: u4
        doc: Image width in pixels
      - id: height
        type: u4
        doc: Image height in pixels
      - id: bit_depth
        type: u1
        doc: Bit depth (1, 2, 4, 8, or 16)
      - id: color_type
        type: u1
        doc: Color type (0, 2, 3, 4, or 6)
        enum: color_type
      - id: compression_method
        type: u1
        doc: Compression method (0 for deflate/inflate)
      - id: filter_method
        type: u1
        doc: Filter method (0 for adaptive filtering)
      - id: interlace_method
        type: u1
        doc: Interlace method (0 for no interlace, 1 for Adam7 interlace)

  plte_chunk:
    seq:
      - id: entries
        type: plte_entry
        repeat: expr
        repeat-expr: (_parent.length / 3)

  plte_entry:
    seq:
      - id: red
        type: u1
        doc: Red component (0-255)
      - id: green
        type: u1
        doc: Green component (0-255)
      - id: blue
        type: u1
        doc: Blue component (0-255)

  idat_chunk:
    seq:
      - id: data
        type: u1
        repeat: expr
        repeat-expr: _parent.length

  iend_chunk:
    seq: []

enums:
  color_type:
    0: greyscale
    2: truecolor
    3: indexed_color
    4: greyscale_alpha
    6: truecolor_alpha