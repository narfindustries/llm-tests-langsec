meta:
  id: png_image
  file-extension: png
  endian: be

seq:
  - id: header
    type: header

  - id: chunks
    type: chunk
    repeat: eos

types:
  header:
    seq:
      - id: signature
        contents: [137, 80, 78, 71, 13, 10, 26, 10]
  
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
      is_ihdr:
        value: type == 'IHDR'
      is_idat:
        value: type == 'IDAT'
      is_iend:
        value: type == 'IEND'