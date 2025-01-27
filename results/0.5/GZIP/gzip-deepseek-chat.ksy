meta:
  id: gzip
  title: GZIP file format
  file-extension: gz
  license: MIT
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
  - id: os
    type: u1
    enum: operating_systems
  - id: extra_field
    type: extra_field
    if: flags & 0x04 != 0
  - id: original_filename
    type: strz
    encoding: UTF-8
    if: flags & 0x08 != 0
  - id: comment
    type: strz
    encoding: UTF-8
    if: flags & 0x10 != 0
  - id: header_crc
    type: u2
    if: flags & 0x02 != 0
  - id: compressed_data
    type: compressed_data
  - id: crc32
    type: u4
  - id: uncompressed_size
    type: u4
types:
  extra_field:
    seq:
      - id: len
        type: u2
      - id: data
        size: len
  compressed_data:
    seq:
      - id: data
        size-eos: true
enums:
  compression_methods:
    0x08: deflate
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
    13: acorn_risc
    255: unknown