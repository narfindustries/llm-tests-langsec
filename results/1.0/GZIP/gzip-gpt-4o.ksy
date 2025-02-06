meta:
  id: gzip
  title: GZIP
  file-extension: gz
  xref:
    rfc: 1952
  license: CC0-1.0
  application: Compressed file format based on deflate algorithm
  endian: le
seq:
  - id: id1
    type: u1
    valid: 0x1f
  - id: id2
    type: u1
    valid: 0x8b
  - id: compression_method
    type: u1
    enum: compression_methods
  - id: flags
    type: u1
    doc: |
      A bit field indicating presence of optional fields
  - id: mtime
    type: u4
    doc: |
      Modification time in UNIX timestamp format (seconds since 1970)
  - id: extra_flags
    type: u1
    enum: extra_flags
  - id: os
    type: u1
    enum: os
  - id: extra
    type: extras
    if: (flags & 0x04) != 0
  - id: file_name
    type: strz
    encoding: UTF-8
    if: (flags & 0x08) != 0
  - id: comment
    type: strz
    encoding: UTF-8
    if: (flags & 0x10) != 0
  - id: header_crc16
    type: u2
    if: (flags & 0x02) != 0
  - id: compressed_blocks
    size-eos: true
    type: raw_data
  - id: crc32
    type: u4
    doc: |
      CRC32 checksum of uncompressed data
  - id: isize
    type: u4
    doc: |
      Size of the original input data modulo 2^32
types:
  extras:
    seq:
      - id: subfields_length
        type: u2
      - id: subfields
        size: subfields_length
        type: bytes
  raw_data:
    seq:
      - id: data
        size-eos: true
        type: u1
enums:
  compression_methods:
    8: deflate
  extra_flags:
    2: maximum_compression
    4: fastest_compression
  os:
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