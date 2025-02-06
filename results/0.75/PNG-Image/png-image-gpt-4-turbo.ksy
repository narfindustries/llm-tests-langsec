meta:
  id: png
  title: Portable Network Graphics (PNG)
  file-extension: png
  endian: be
  license: CC0-1.0
  ks-version: 0.9

doc: |
  PNG is a bitmap image format that employs lossless data compression.
  PNG was created to improve upon and replace GIF as an image-file format
  not requiring a patent license.

seq:
  - id: signature
    contents: [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

  - id: chunks
    type: chunk
    repeat: eos

types:
  chunk:
    seq:
      - id: length
        type: u4
      - id: type
        type: str
        encoding: ASCII
        size: 4
      - id: data
        size: length
        type:
          switch-on: type
          cases:
            '"IHDR"': ihdr_chunk
            '"PLTE"': plte_chunk
            '"IDAT"': idat_chunk
            '"IEND"': iend_chunk
            '"tRNS"': trns_chunk
            '"cHRM"': chrm_chunk
            '"gAMA"': gama_chunk
            '"iTXt"': itxt_chunk
            '"tEXt"': text_chunk
            '"zTXt"': ztxt_chunk
            '"bKGD"': bkgd_chunk
            '"pHYs"': phys_chunk
            '"sBIT"': sbit_chunk
            '"sRGB"': srgb_chunk
            '"hIST"': hist_chunk
            '"tIME"': time_chunk
      - id: crc
        type: u4

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

  plte_chunk:
    seq:
      - id: entries
        type: rgb
        repeat: expr
        repeat-expr: _parent.length / 3

  idat_chunk:
    seq:
      - id: data
        size-eos: true

  iend_chunk:
    doc: Marks the end of the PNG datastream

  trns_chunk:
    doc: Transparency information

  chrm_chunk:
    doc: Primary chromaticities and white point

  gama_chunk:
    doc: Image gamma information

  itxt_chunk:
    doc: Compressed international textual data

  text_chunk:
    doc: Uncompressed international textual data

  ztxt_chunk:
    doc: Compressed textual data

  bkgd_chunk:
    doc: Background color

  phys_chunk:
    doc: Physical pixel dimensions

  sbit_chunk:
    doc: Significant bits

  srgb_chunk:
    doc: Standard RGB color space

  hist_chunk:
    doc: Image histogram

  time_chunk:
    doc: Image last-modification time

  rgb:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1