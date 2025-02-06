meta:
  id: gzip
  file-extension: gz
  endian: le

seq:
  - id: magic
    contents: [0x1f, 0x8b]
  - id: compression_method
    type: u1
    enum: compression
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
    encoding: iso-8859-1
    if: flags.name
  - id: comment
    type: strz
    encoding: iso-8859-1
    if: flags.comment
  - id: header_crc16
    type: u2
    if: flags.hcrc
  - id: compressed_data
    size-eos: true
    consume: false
  - id: footer
    type: footer

types:
  flags:
    seq:
      - id: reserved
        type: b3
      - id: comment
        type: b1
      - id: name
        type: b1
      - id: extra
        type: b1
      - id: hcrc
        type: b1
      - id: text
        type: b1

  extra_field:
    seq:
      - id: xlen
        type: u2
      - id: extra_data
        size: xlen

  footer:
    seq:
      - id: crc32
        type: u4
      - id: isize
        type: u4

enums:
  compression:
    8: deflate

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