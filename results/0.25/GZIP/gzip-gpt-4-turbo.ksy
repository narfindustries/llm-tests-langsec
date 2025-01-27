meta:
  id: gzip
  title: Gzip (GNU zip) is a compression format
  file-extension: gz
  endian: le
  license: GPL-2.0+
doc: |
  Gzip is a popular compression format used in various Internet applications.
  This specification describes the structure of a gzip file.
seq:
  - id: magic
    contents: [0x1f, 0x8b]
  - id: compression_method
    type: u1
    enum: compression_methods
    doc: Compression method used (only deflate is supported by this specification)
  - id: flags
    type: flags
  - id: mod_time
    type: u4
    doc: Modification time of the original file
  - id: extra_flags
    type: u1
    enum: extra_flags
    doc: Extra flags, depend on compression method
  - id: os
    type: u1
    enum: operating_systems
    doc: Operating system on which the file was compressed
  - id: extras
    type: extras
    if: flags.has_extra
  - id: original_filename
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
    doc: The actual compressed data
  - id: crc32
    type: u4
    doc: CRC-32 checksum of the uncompressed data
  - id: uncompressed_size
    type: u4
    doc: Size of the uncompressed data, modulo 2^32

types:
  flags:
    seq:
      - id: has_text
        type: b1
      - id: has_crc
        type: b1
      - id: has_extra
        type: b1
      - id: has_name
        type: b1
      - id: has_comment
        type: b1
      - id: is_encrypted
        type: b1
      - id: is_reserved
        type: b7

  extras:
    seq:
      - id: len
        type: u2
      - id: data
        size: len

enums:
  compression_methods:
    8: deflate

  extra_flags:
    2: maximum
    4: fast
    6: super_fast

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