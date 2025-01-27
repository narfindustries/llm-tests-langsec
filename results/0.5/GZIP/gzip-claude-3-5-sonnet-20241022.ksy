meta:
  id: gzip
  file-extension: gz
  endian: le

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
  - id: operating_system
    type: u1
    enum: operating_systems
  - id: extra_fields
    type: extra_field
    if: flags & 0x04 != 0
  - id: name
    type: strz
    encoding: utf-8
    if: flags & 0x08 != 0
  - id: comment
    type: strz
    encoding: utf-8
    if: flags & 0x10 != 0
  - id: header_crc16
    type: u2
    if: flags & 0x02 != 0
  - id: compressed_data
    size-eos: true
    process: zlib
    if: compression_method == compression_methods::deflate

types:
  extra_field:
    seq:
      - id: len_data
        type: u2
      - id: extra_data
        size: len_data

enums:
  compression_methods:
    0: store
    8: deflate

  operating_systems:
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