meta:
  id: gzip
  title: Gzip Archive
  file-extension: gz
  endian: le
  license: CC0-1.0
seq:
  - id: magic
    contents: [0x1f, 0x8b]
  - id: compression_method
    type: u1
    enum: compression_methods
  - id: flags
    type: flags
  - id: mod_time
    type: u4
  - id: extra_flags
    type: u1
    enum: extra_flags
  - id: os
    type: u1
    enum: operating_systems
  - id: extras
    type: extras
    if: flags.has_extra
  - id: name
    type: strz
    encoding: ASCII
    if: flags.has_name
  - id: comment
    type: strz
    encoding: ASCII
    if: flags.has_comment
  - id: header_crc16
    type: u2
    if: flags.has_crc
  - id: compressed_data
    size-eos: true
    type: deflate_blocks
  - id: crc32
    type: u4
  - id: input_size
    type: u4
types:
  flags:
    seq:
      - id: reserved
        type: b3
      - id: has_comment
        type: b1
      - id: has_name
        type: b1
      - id: has_extra
        type: b1
      - id: has_crc
        type: b1
      - id: is_text
        type: b1
  extras:
    seq:
      - id: num_extra_fields
        type: u2
      - id: extra_fields
        type: extra_field
        repeat: expr
        repeat-expr: num_extra_fields
  extra_field:
    seq:
      - id: subfield_id
        type: u2
      - id: len_subfield_data
        type: u2
      - id: subfield_data
        size: len_subfield_data
  deflate_blocks:
    seq:
      - id: data
        size-eos: true
enums:
  compression_methods:
    8: deflate
  extra_flags:
    2: maximum_compression
    4: fastest_compression
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