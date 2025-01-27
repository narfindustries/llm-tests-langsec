meta:
  id: gzip
  file-extension: gz
  endian: le
seq:
  - id: header
    type: header
  - id: compressed_data
    type: compressed_data
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
      - id: modification_time
        type: u4
      - id: extra_flags
        type: u1
      - id: os
        type: u1
        enum: operating_systems
  compressed_data:
    seq:
      - id: data
        type: bytes
        size-eos: true
  footer:
    seq:
      - id: crc32
        type: u4
      - id: uncompressed_size
        type: u4
enums:
  compression_methods:
    8: deflate
  operating_systems:
    0: fat
    3: unix
    7: macintosh