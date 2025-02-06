meta:
  id: gzip
  file-extension: gz
  endian: le
seq:
  - id: id1
    type: u1
    valid: 0x1f
  - id: id2
    type: u1
    valid: 0x8b
  - id: cm
    type: u1
    valid: 8
  - id: flg
    type: u1
  - id: mtime
    type: u4
  - id: xfl
    type: u1
  - id: os
    type: u1
  - id: extra
    type: extra_field
    if: 'flg & 4 != 0'
  - id: fname
    type: strz
    encoding: UTF-8
    if: 'flg & 8 != 0'
  - id: fcomment
    type: strz
    encoding: UTF-8
    if: 'flg & 16 != 0'
  - id: hcrc
    type: u2
    if: 'flg & 2 != 0'
  - id: compressed_data
    type: compressed_data
  - id: crc32
    type: u4
  - id: isize
    type: u4
types:
  extra_field:
    seq:
      - id: xlen
        type: u2
      - id: data
        size: xlen
  compressed_data:
    seq:
      - id: data
        size-eos: true
  strz:
    seq:
      - id: value
        type: str
        encoding: UTF-8
        terminator: 0