meta:
  id: gzip
  title: GZIP
  file-extension: gz
  xref:
    rfc: 1952
  license: MIT
  endian: little

seq:
  - id: magic
    contents: [0x1f, 0x8b]
  - id: compression_method
    type: u1
    enum: compression_methods
  - id: flags
    type: u1
    enum: flags
  - id: mod_time
    type: u4
  - id: extra_flags
    type: u1
  - id: os
    type: u1
    enum: oses
  - id: extra
    type: extra_field
    if: flags.has_extra
  - id: name
    type: strz
    encoding: UTF-8
    if: flags.has_name
  - id: comment
    type: strz
    encoding: UTF-8
    if: flags.has_comment
  - id: header_crc16
    type: u2
    if: flags.has_header_crc
  - id: body
    size-eos: true

types:
  extra_field:
    seq:
      - id: subfields
        repeat: eos
        type: subfield

  subfield:
    seq:
      - id: id
        type: u2
      - id: len_data
        type: u2
      - id: data
        size: len_data

enums:
  compression_methods:
    deflate: 8

  flags:
    0: reserved
    1: has_text
    2: has_header_crc
    4: has_extra
    8: has_name
    16: has_comment

  oses:
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