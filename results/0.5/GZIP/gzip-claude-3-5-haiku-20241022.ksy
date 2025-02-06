meta:
  id: gzip
  file-extension: gz
  endian: le
seq:
  - id: identification
    contents: [0x1F, 0x8B]
  - id: compression_method
    type: u1
    enum: compression_methods
  - id: flags
    type: flags
  - id: mtime
    type: u4
  - id: extra_flags
    type: u1
  - id: os
    type: u1
    enum: operating_systems
  - id: extra_field
    type: extra_field
    if: flags.fextra
  - id: filename
    type: strz
    encoding: UTF-8
    if: flags.fname
  - id: comment
    type: strz
    encoding: UTF-8
    if: flags.fcomment
  - id: header_crc
    type: u2
    if: flags.fhcrc
  - id: compressed_data
    type: deflate_block
  - id: crc32
    type: u4
  - id: uncompressed_size
    type: u4

types:
  flags:
    seq:
      - id: ftext
        type: b1
      - id: fhcrc
        type: b1
      - id: fextra
        type: b1
      - id: fname
        type: b1
      - id: fcomment
        type: b1
      - id: fencrypt
        type: b1
      - id: reserved
        type: b2

  extra_field:
    seq:
      - id: length
        type: u2
      - id: data
        size: length

  deflate_block:
    seq:
      - id: raw_data
        type: byte_array

  byte_array:
    seq:
      - id: data
        type: u1
        repeat: eos

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