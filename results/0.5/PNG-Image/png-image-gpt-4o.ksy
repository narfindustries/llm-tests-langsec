meta:
  id: png_image
  title: PNG Image
  file-extension: png
  xref:
    mime: image/png
  endian: be
  encoding: utf-8

doc: |
  Portable Network Graphics (PNG) is a raster-graphics file-format that supports lossless data compression.

seq:
  - id: signature
    contents: [137, 80, 78, 71, 13, 10, 26, 10]

  - id: chunks
    type: chunk
    repeat: until
    repeat-until: _.type == "IEND"

types:
  chunk:
    seq:
      - id: length
        type: u4
      - id: type
        type: str
        size: 4
      - id: body
        size: length
        if: type != "IEND"
      - id: crc
        type: u4

    instances:
      body_parsed:
        value: 
          switch-on: type
          cases:
            "IHDR": ihdr_chunk
            "PLTE": plte_chunk
            "IDAT": idat_chunk
            "IEND": iend_chunk

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
        type: rgb_entry
        repeat: eos

  rgb_entry:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1

  idat_chunk:
    seq:
      - id: compressed_data
        size-eos: true

  iend_chunk:
    doc: "Marks the end of the PNG file."