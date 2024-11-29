meta:
  id: png
  title: PNG (Portable Network Graphics) Image Format
  file-extension: 
    - png
  license: ISO/IEC 15948:2004
  endian: big

seq:
  - id: signature
    contents: [137, 80, 78, 71, 13, 10, 26, 10]
    doc: PNG file signature (magic number)

  - id: chunks
    type: chunk
    repeat: eos
    doc: Sequence of PNG chunks that define the image structure and data

types:
  chunk:
    seq:
      - id: length
        type: u4
        doc: Length of chunk data in bytes

      - id: chunk_type
        type: str
        encoding: ASCII
        size: 4
        doc: Four-letter chunk type identifier

      - id: data
        size: length
        type:
          switch-on: chunk_type
          cases:
            "'IHDR'": ihdr_chunk
            "'PLTE'": palette_chunk
            "'IDAT'": image_data_chunk
            "'IEND'": end_chunk
        doc: Chunk-specific data based on chunk type

      - id: crc
        type: u4
        doc: CRC32 checksum of chunk type and data

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
        doc: Bits per sample or palette index

      - id: color_type
        type: u1
        enum: color_types
        doc: Color type defining image color interpretation

      - id: compression_method
        type: u1
        enum: compression_methods
        doc: Compression method used for image data

      - id: filter_method
        type: u1
        enum: filter_methods
        doc: Filter method used before compression

      - id: interlace_method
        type: u1
        enum: interlace_methods
        doc: Interlacing method for image data

  palette_chunk:
    seq:
      - id: palette_entries
        type: rgb
        repeat: eos
        doc: RGB color palette entries

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
      - id: compressed_data
        type: bytes
        doc: Compressed image data using DEFLATE algorithm

  end_chunk:
    seq: []
    doc: Marks the end of the PNG file, contains no data

enums:
  color_types:
    0: grayscale
    2: rgb
    3: palette
    4: grayscale_alpha
    6: rgba

  compression_methods:
    0: deflate_inflate

  filter_methods:
    0: standard_png_filtering

  interlace_methods:
    0: no_interlace
    1: adam7_interlace