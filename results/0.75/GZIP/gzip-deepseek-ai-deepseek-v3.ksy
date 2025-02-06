meta:
  id: gzip
  file-extension: gz
  endian: le
seq:
  - id: header
    type: gzip_header
  - id: compressed_data
    size-eos: true
  - id: trailer
    type: gzip_trailer
types:
  gzip_header:
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
      - id: modification_time
        type: u4
      - id: extra_flags
        type: u1
      - id: os
        type: u1
        enum: os_types
      - id: extra_field
        type: extra_field
        if: '(flags & 4) != 0'
      - id: original_filename
        type: str
        encoding: UTF-8
        terminator: 0
        if: '(flags & 8) != 0'
      - id: comment
        type: str
        encoding: UTF-8
        terminator: 0
        if: '(flags & 16) != 0'
      - id: header_crc16
        type: u2
        if: '(flags & 2) != 0'
  extra_field:
    seq:
      - id: xlen
        type: u2
      - id: data
        size: xlen
  gzip_trailer:
    seq:
      - id: crc32
        type: u4
      - id: isize
        type: u4
enums:
  compression_methods:
    8: deflate
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
    9: cp_m
    10: tops_20
    11: ntfs
    12: qdos
    13: acorn_riscos
    255: unknown