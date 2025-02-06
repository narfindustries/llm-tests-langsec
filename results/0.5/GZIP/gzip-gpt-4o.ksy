meta:
  id: gzip
  title: GZIP
  file-extension: gz
  xref:
    rfc: 1952
  license: CC0-1.0
  endian: le

seq:
  - id: id1
    type: u1
    valid: 0x1F
  - id: id2
    type: u1
    valid: 0x8B
  - id: compression_method
    type: u1
    enum: compression_methods
  - id: flags
    type: u1
  - id: mtime
    type: u4
    doc: Modification time (Unix timestamp)
  - id: extra_flags
    type: u1
    enum: extra_flags
  - id: os
    type: u1
    enum: os
  - id: extra
    type: extra_field
    if: (flags & 0x04) != 0
  - id: original_file_name
    type: strz
    encoding: ASCII
    if: (flags & 0x08) != 0
  - id: file_comment
    type: strz
    encoding: ASCII
    if: (flags & 0x10) != 0
  - id: header_crc16
    type: u2
    if: (flags & 0x02) != 0
  - id: compressed_data
    size-eos: true
    doc: Compressed data
  - id: crc32
    type: u4
    doc: CRC32 of uncompressed data
  - id: isize
    type: u4
    doc: Size of uncompressed data modulo 2^32

types:
  extra_field:
    seq:
      - id: subfield_id
        type: u2
      - id: len_subfield_data
        type: u2
      - id: subfield_data
        size: len_subfield_data

enums:
  compression_methods:
    8: deflate

  extra_flags:
    0x02: maximum_compression
    0x04: fastest_compression

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