meta:
  id: gzip
  title: Gzip File Format
  file-extension: gz
  endian: le
seq:
  - id: magic
    contents: [0x1f, 0x8b]
  - id: compression_method
    type: u1
    enum: compression_methods
  - id: flags
    type: b8
  - id: mtime
    type: u4
  - id: extra_flags
    type: u1
    enum: extra_flags
  - id: os
    type: u1
    enum: os
  - id: extras
    type: extras
    if: flags & 0b00000100 != 0
  - id: filename
    type: strz
    encoding: ASCII
    if: flags & 0b00001000 != 0
  - id: comment
    type: strz
    encoding: ASCII
    if: flags & 0b00010000 != 0
  - id: hcrc
    type: u2
    if: flags & 0b00000010 != 0
  - id: compressed_data
    size-eos: true
  - id: crc32
    type: u4
  - id: isize
    type: u4

types:
  extras:
    seq:
      - id: len_extra
        type: u2
      - id: extra_fields
        size: len_extra

enums:
  compression_methods:
    8: deflate
  extra_flags:
    2: max_compression
    4: fastest_algorithm
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