meta:
  id: gzip
  title: GZIP
  file-extension: gz
  xref:
    rfc: 1952
  license: CC0-1.0
seq:
  - id: magic
    contents: [ 0x1f, 0x8b ]
  - id: compression_method
    type: u1
    enum: compression_methods
  - id: flags
    type: u1
  - id: mtime
    type: u4le
  - id: extra_flags
    type: u1
    enum: extra_flags
  - id: os
    type: u1
    enum: os_types
  - id: extra_len
    type: u2le
    if: (flags & 4) != 0
  - id: extra
    size: extra_len
    if: (flags & 4) != 0
    type: extra_field
  - id: original_file_name
    type: strz
    encoding: UTF-8
    if: (flags & 8) != 0
  - id: file_comment
    type: strz
    encoding: UTF-8
    if: (flags & 16) != 0
  - id: header_crc16
    type: u2le
    if: (flags & 2) != 0
  - id: compressed_data
    size-eos: true
    type: compressed_block
types:
  extra_field:
    seq:
      - id: subfields
        type: subfield
        repeat: eos
  subfield:
    seq:
      - id: id
        type: u2le
      - id: len_data
        type: u2le
      - id: data
        size: len_data
  compressed_block:
    seq:
      - id: blocks
        type: u1
        repeat: eos
    instances:
      crc32:
        pos: -8
        type: u4le
      isize:
        pos: -4
        type: u4le
enums:
  compression_methods:
    8: deflate
  extra_flags:
    2: maximum_compression
    4: fastest_compression
  os_types:
    0: fat
    1: amiga
    2: vms
    3: unix
    4: vm_cms
    5: atari_tos
    6: hpfs
    7: macintosh
    8: z_system
    9: cpm
    10: tops_20
    11: ntfs
    12: qdos
    13: acorn_riscos
    255: unknown