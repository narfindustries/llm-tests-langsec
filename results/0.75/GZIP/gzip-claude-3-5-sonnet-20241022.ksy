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
    enum: extra_flags
  - id: operating_system
    type: u1
    enum: operating_system
  - id: extra_field
    type: extra_field
    if: flags.flag_byte & 0x04 != 0
  - id: name
    type: strz
    encoding: utf-8
    if: flags.flag_byte & 0x08 != 0
  - id: comment
    type: strz
    encoding: utf-8
    if: flags.flag_byte & 0x10 != 0
  - id: header_crc16
    type: u2
    if: flags.flag_byte & 0x02 != 0
  - id: compressed_data
    size-eos: true
  - id: crc32
    type: u4
  - id: isize
    type: u4

types:
  flags:
    seq:
      - id: flag_byte
        type: u1
    instances:
      reserved3:
        value: (flag_byte >> 7) & 1
      reserved2:
        value: (flag_byte >> 6) & 1
      reserved1:
        value: (flag_byte >> 5) & 1
      flag_comment:
        value: (flag_byte >> 4) & 1
      flag_name:
        value: (flag_byte >> 3) & 1
      flag_extra:
        value: (flag_byte >> 2) & 1
      flag_hcrc:
        value: (flag_byte >> 1) & 1
      flag_text:
        value: flag_byte & 1

  extra_field:
    seq:
      - id: len_extra_data
        type: u2
      - id: extra_data
        size: len_extra_data

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