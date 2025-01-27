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
      - id: chunk_type
        type: str
        size: 4
        encoding: ASCII
      - id: chunk_data
        size: length
      - id: crc
        type: u4
    instances:
      is_ihdr:
        value: chunk_type == 'IHDR'
      is_idat:
        value: chunk_type == 'IDAT'
      is_iend:
        value: chunk_type == 'IEND'