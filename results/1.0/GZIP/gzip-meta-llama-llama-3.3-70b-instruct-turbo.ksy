meta:
  id: gzip
  file-extension: gz
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
        size: 2
      - id: compression_method
        size: 1
      - id: flags
        size: 1
      - id: modification_time
        size: 4
      - id: extra_flags
        size: 1
      - id: operating_system
        size: 1
    instances:
      is_gzip:
        value: magic == [0x1f, 0x8b]
  body:
    seq:
      - id: compressed_data
        process: xz
        size: until-eof
  footer:
    seq:
      - id: crc32
        size: 4
      - id: uncompressed_size
        size: 4