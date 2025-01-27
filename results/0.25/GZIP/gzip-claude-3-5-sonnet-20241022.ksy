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
    if: flags & flags_enum::extra.to_i != 0
  - id: name
    type: strz
    encoding: utf-8
    if: flags & flags_enum::name.to_i != 0
  - id: comment
    type: strz
    encoding: utf-8
    if: flags & flags_enum::comment.to_i != 0
  - id: header_crc16
    type: u2
    if: flags & flags_enum::header_crc.to_i != 0
  - id: compressed_data
    size-eos: true
    if: not _io.eof

types:
  extra_field:
    seq:
      - id: len_subfields
        type: u2
      - id: subfields
        type: subfield
        repeat: eos

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
    0: store
    1: compress
    2: pack
    3: lzh
    4: reserved_4
    5: reserved_5
    6: reserved_6
    7: reserved_7
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

  flags_enum:
    1: text
    2: header_crc
    4: extra
    8: name
    16: comment