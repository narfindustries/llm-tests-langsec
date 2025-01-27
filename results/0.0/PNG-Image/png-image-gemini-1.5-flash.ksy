meta:
  id: png-image-gemini-1
  title: PNG Image (Gemini 1.5 Flash)
  homepage: https://kaitai.io
  file-extension: png
  experimental: true

types:
  png_image:
    seq:
      - id: signature
        type: str
        size: 8
      - id: ihdr
        type: ihdr
      - id: idat
        type: idat_list
      - id: iend
        type: iend

  ihdr:
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

  idat_list:
    seq:
      - id: idat_entries
        type: idat_entry+

  idat_entry:
    seq:
      - id: length
        type: u4
      - id: type
        type: str
        size: 4
      - id: data
        type: bytes
        size: length
      - id: crc
        type: u4

  iend:
    seq:
      - id: length
        type: u4
      - id: type
        type: str
        size: 4
      - id: crc
        type: u4

