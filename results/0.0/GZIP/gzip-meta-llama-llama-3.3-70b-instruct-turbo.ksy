meta:
  endian: le
seq:
  - id: id1
    type: u1
  - id: id2
    type: u1
  - id: cm
    type: u1
  - id: flags
    type: u1
  - id: mtime
    type: u4
  - id: xfl
    type: u1
  - id: os
    type: u1
  - id: extra
    type:
      seq:
        - id: xlen
          type: u2
        - id: extra_field
          type: bytes
          size: xlen
    if: flags & 4 != 0
  - id: fname
    type:
      str:
        encoding: ascii
        terminator: 0
    if: flags & 8 != 0
  - id: fcomment
    type:
      str:
        encoding: ascii
        terminator: 0
    if: flags & 16 != 0
  - id: hcrc
    type: u2
    if: flags & 2 != 0
  - id: compr
    type: bytes
    size: eos
  - id: isize
    type: u4
  - id: crc32
    type: u4