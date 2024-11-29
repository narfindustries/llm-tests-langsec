meta:
  id: png
  title: Portable Network Graphics (PNG)
  application: Image files
  file-extension: png
  endian: big
  description: |
    This is a Kaitai Struct specification for the PNG file format,
    as described in ISO/IEC 15948:2004. PNG uses a chunk-based structure.

seq:
  - id: signature
    type: bytes
    size: 8
    contents: [137, 80, 78, 71, 13, 10, 26, 10]
    description: PNG file signature.

  - id: chunks
    type: chunk
    repeat: until
    repeat-until: _.type == "IEND"
    description: Sequence of chunks making up the PNG file.

types:
  chunk:
    seq:
      - id: length
        type: u4
        description: Length of the chunk data in bytes.

      - id: type
        type: str
        size: 4
        description: Chunk type as an ASCII string (e.g., IHDR, IDAT, IEND).

      - id: body
        size: length
        type:
          switch-on: type
          cases:
            '"IHDR"': ihdr_chunk
            '"PLTE"': plte_chunk
            '"IDAT"': idat_chunk
            '"IEND"': iend_chunk
        description: The chunk-specific data.

      - id: crc
        type: u4
        description: CRC-32 checksum of the chunk type and body.

  ihdr_chunk:
    seq:
      - id: width
        type: u4
        description: Image width in pixels.

      - id: height
        type: u4
        description: Image height in pixels.

      - id: bit_depth
        type: u1
        description: Number of bits per sample or per palette index.

      - id: color_type
        type: u1
        description: |
          Color type: 
          0 = Grayscale, 
          2 = Truecolor, 
          3 = Indexed-color, 
          4 = Grayscale with alpha, 
          6 = Truecolor with alpha.

      - id: compression_method
        type: u1
        description: Compression method (0 = deflate).

      - id: filter_method
        type: u1
        description: Filter method (0 = adaptive filtering).

      - id: interlace_method
        type: u1
        description: Interlace method (0 = no interlace, 1 = Adam7).

  plte_chunk:
    seq:
      - id: palette_entries
        type: palette_entry
        repeat: expr
        repeat-expr: _parent.length / 3
        description: RGB palette entries.

  palette_entry:
    seq:
      - id: red
        type: u1
        description: Red intensity (0-255).

      - id: green
        type: u1
        description: Green intensity (0-255).

      - id: blue
        type: u1
        description: Blue intensity (0-255).

  idat_chunk:
    seq:
      - id: compressed_data
        size: _parent.length
        description: Compressed image data (deflate stream).

  iend_chunk:
    description: Marks the end of the PNG file. Contains no data.
    seq: []
