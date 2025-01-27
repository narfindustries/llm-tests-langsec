meta:
  id: gzip
  title: Gzip (GNU zip) archive data format
  file-extension: gz
  endian: le
  license: GPL-2.0+
  ks-version: 0.9
doc: |
  Gzip is a file format used for file compression and decompression. The format
  was created by the GNU project in 1992, and is used by the gzip and gunzip utilities.
  Gzip typically uses the DEFLATE algorithm to compress files, and it can include a header
  with metadata such as the original file name and timestamp.
seq:
  - id: magic
    contents: [0x1f, 0x8b]
  - id: compression_method
    type: u1
    enum: compression_methods
    valid:
      eq: compression_methods.deflate
  - id: flags
    type: flags
  - id: mod_time
    type: u4
  - id: extra_flags
    type: u1
  - id: os
    type: u1
    enum: operating_systems
  - id: extras
    type: extras
    if: flags.has_extra
  - id: original_file_name
    type: strz
    encoding: UTF-8
    if: flags.has_name
  - id: comment
    type: strz
    encoding: UTF-8
    if: flags.has_comment
  - id: header_crc16
    type: u2
    if: flags.has_crc
  - id: compressed_data
    size-eos: true
  - id: footer
    type: footer

types:
  flags:
    seq:
      - id: reserved
        type: b3
      - id: has_crc
        type: b1
      - id: has_extra
        type: b1
      - id: has_name
        type: b1
      - id: has_comment
        type: b1
      - id: is_text
        type: b1

  extras:
    seq:
      - id: len_extra
        type: u2
      - id: extra_fields
        type: extra_field
        repeat: expr
        repeat-expr: len_extra

  extra_field:
    seq:
      - id: si1
        type: u1
      - id: si2
        type: u1
      - id: len_field
        type: u2
      - id: field_data
        size: len_field

  footer:
    seq:
      - id: crc32
        type: u4
      - id: input_size
        type: u4

enums:
  compression_methods:
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