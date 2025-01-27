meta:
  id: gzip
  file-extension: gz
  title: GZIP file format
  endian: le
seq:
  - id: header
    type: header
  - id: body
    type: body
  - id: footer
    type: footer
types:
  header:
    seq:
      - id: magic
        contents: [0x1f, 0x8b]
      - id: compression_method
        type: u1
        enum: compression_methods
      - id: flags
        type: u1
        # Bitfield for various header flags
      - id: modification_time
        type: u4
      - id: extra_flags
        type: u1
      - id: os
        type: u1
        enum: operating_systems
  body:
    seq:
      - id: compressed_data
        type: str
        size-eos: true
  footer:
    seq:
      - id: crc32
        type: u4
      - id: original_size
        type: u4
enums:
  compression_methods:
    8: deflate
  operating_systems:
    0: fat
    3: unix
    7: macintosh
    11: ntfs