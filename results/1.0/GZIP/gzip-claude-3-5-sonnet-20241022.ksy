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
    enum: compression_method
  - id: flags
    type: flags
  - id: mtime
    type: u4
  - id: extra_flags
    type: u1
  - id: operating_system
    type: u1
    enum: os
  - id: extra_field
    type: extra_field
    if: flags.extra
  - id: name
    type: strz
    encoding: utf-8
    if: flags.name
  - id: comment
    type: strz
    encoding: utf-8
    if: flags.comment
  - id: header_crc16
    type: u2
    if: flags.header_crc
  - id: compressed_data
    type: compressed_data

types:
  flags:
    seq:
      - id: reserved2
        type: b1
      - id: reserved1
        type: b1
      - id: reserved0
        type: b1
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

  extra_field:
    seq:
      - id: xlen
        type: u2
      - id: extra_data
        size: xlen

  compressed_data:
    seq:
      - id: data
        size: _io.size - 8
      - id: crc32
        type: u4
      - id: isize
        type: u4

enums:
  compression_method:
    0: reserved0
    1: reserved1
    2: reserved2
    3: reserved3
    4: reserved4
    5: reserved5
    6: reserved6
    7: reserved7
    8: deflate
    9: reserved9
    10: reserved10
    11: reserved11
    12: reserved12
    13: reserved13
    14: reserved14
    15: reserved15

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