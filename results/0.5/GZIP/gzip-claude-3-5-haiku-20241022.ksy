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
    instances:
      extra_field:
        type: extra_field
        if: (flags & 0b00000100) != 0
      filename:
        type: str
        encoding: UTF-8
        terminator: 0
        if: (flags & 0b00001000) != 0
      comment:
        type: str
        encoding: UTF-8
        terminator: 0
        if: (flags & 0b00010000) != 0
      header_crc16:
        type: u2
        if: (flags & 0b00000010) != 0
  extra_field:
    seq:
      - id: length
        type: u2
      - id: data
        size: length
  body:
    seq:
      - id: compressed_data
        type: bytes
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
    1: amiga
    2: vms
    3: unix
    4: vm_cms
    5: atari
    6: hpfs
    7: macintosh
    8: z_system
    9: cp_m
    10: tops_20
    11: ntfs
    12: qdos
    13: acorn_riscos
    255: unknown