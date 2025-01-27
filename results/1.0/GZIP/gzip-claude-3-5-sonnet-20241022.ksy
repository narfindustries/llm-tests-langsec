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
    type: flags_struct
  - id: modification_time
    type: u4
  - id: extra_flags
    type: u1
  - id: operating_system
    type: u1
    enum: operating_systems
  - id: compressed_data
    size-eos: true
    type: compressed_data
    if: not _io.eof

types:
  flags_struct:
    seq:
      - id: text
        type: b1
      - id: header_crc
        type: b1
      - id: extra
        type: b1
      - id: name
        type: b1
      - id: comment
        type: b1
      - id: reserved
        type: b3

  compressed_data:
    seq:
      - id: extra_fields
        type: extra_field
        if: _parent.flags.extra
        repeat: until
        repeat-until: _.id == 0
      - id: name
        type: strz
        encoding: utf-8
        if: _parent.flags.name
      - id: comment
        type: strz
        encoding: utf-8
        if: _parent.flags.comment
      - id: header_crc16
        type: u2
        if: _parent.flags.header_crc
      - id: deflate_data
        size-eos: true

  extra_field:
    seq:
      - id: id
        type: u2
      - id: len
        type: u2
      - id: data
        size: len

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