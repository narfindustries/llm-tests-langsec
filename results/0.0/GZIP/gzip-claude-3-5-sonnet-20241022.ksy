meta:
  id: gzip
  file-extension: gz
  endian: le
seq:
  - id: magic1
    contents: [0x1f]
  - id: magic2
    contents: [0x8b]
  - id: compression_method
    type: u1
    enum: compression
  - id: flags
    type: flags
  - id: mtime
    type: u4
  - id: extra_flags
    type: u1
    enum: extra_flags
  - id: os
    type: u1
    enum: os
  - id: extra_length
    type: u2
    if: flags.extra
  - id: extra
    size: extra_length
    if: flags.extra
  - id: name
    type: strz
    encoding: ASCII
    if: flags.name
  - id: comment
    type: strz
    encoding: ASCII
    if: flags.comment
  - id: header_crc16
    type: u2
    if: flags.header_crc
  - id: compressed_data
    size: _io.size - _io.pos - 8
  - id: crc32
    type: u4
  - id: isize
    type: u4
types:
  flags:
    meta:
      bit-endian: le
    seq:
      - id: reserved
        type: b3
      - id: comment
        type: b1
      - id: name
        type: b1
      - id: extra
        type: b1
      - id: header_crc
        type: b1
      - id: text
        type: b1
enums:
  compression:
    0: store
    8: deflate
  extra_flags:
    0: none
    2: maximum_compression
    4: fastest_compression
  os:
    0: fat
    1: amiga
    2: vms
    3: unix
    4: vm_cms
    5: atari_tos
    6: hpfs
    7: macintosh
    8: z_system
    9: cp_m
    10: tops_20
    11: ntfs
    12: qdos
    13: acorn_riscos
    255: unknown