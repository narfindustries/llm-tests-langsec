meta:
  id: gzip
  title: GZIP compressed file
  file-extension: gz
  xref:
    rfc: 1952
  endian: le

seq:
  - id: magic
    contents: [0x1f, 0x8b]
  - id: compression_method
    type: u1
    enum: compression_method
  - id: flags
    type: flags
  - id: mtime
    type: u4
  - id: extra_flags
    type: u1
    enum: extra_flags
  - id: operating_system
    type: u1
    enum: operating_system
  - id: extra_field
    type: extra_field
    if: flags.extra
  - id: name
    type: strz
    encoding: iso-8859-1
    if: flags.name
  - id: comment
    type: strz
    encoding: iso-8859-1
    if: flags.comment
  - id: header_crc16
    type: u2
    if: flags.header_crc
  - id: compressed_data
    size: _io.size - 8 - _io.pos
    process: zlib
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

  extra_field:
    seq:
      - id: xlen
        type: u2
      - id: extra_data
        size: xlen

enums:
  compression_method:
    0: reserved_0
    1: reserved_1
    2: reserved_2
    3: reserved_3
    4: reserved_4
    5: reserved_5
    6: reserved_6
    7: reserved_7
    8: deflate
    9: reserved_9
    10: reserved_10
    11: reserved_11
    12: reserved_12
    13: reserved_13
    14: reserved_14
    15: reserved_15

  extra_flags:
    2: maximum_compression
    4: fastest_compression

  operating_system:
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