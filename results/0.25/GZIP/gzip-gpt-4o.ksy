meta:
  id: gzip
  title: GZIP
  file-extension: gz
  xref:
    rfc: 1952
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
    valid: 0x08
  - id: flags
    type: u1
    enum: flags
  - id: mtime
    type: u4
  - id: extra_flags
    type: u1
    enum: extra_flags
  - id: os
    type: u1
    enum: os
  - id: extra
    type: extra_field
    if: flags.fextra
  - id: original_file_name
    type: strz
    encoding: UTF-8
    if: flags.fname
  - id: file_comment
    type: strz
    encoding: UTF-8
    if: flags.fcomment
  - id: header_crc16
    type: u2
    if: flags.fhcrc
  - id: compressed_data
    size: _io.size - _io.pos - 8
    type: b
  - id: crc32
    type: u4
  - id: isize
    type: u4

types:
  extra_field:
    seq:
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
        type: b

enums:
  flags:
    0: ftext
    1: fhcrc
    2: fextra
    3: fname
    4: fcomment

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