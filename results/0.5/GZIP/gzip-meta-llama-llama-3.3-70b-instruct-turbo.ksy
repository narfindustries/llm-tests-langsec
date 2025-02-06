meta:
  id: gzip
  endian: be
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
    type: u4be
  - id: xfl
    type: u1
  - id: os
    type: u1
  - id: extra
    type:
      seq:
        - id: xlen
          type: u2be
          if: flags & 4 != 0
        - id: extra_data
          type: bytes
          size: xlen
          if: flags & 4 != 0
  - id: name
    type:
      str:
        encoding: ascii
        terminator: 0
        size: eos
      if: flags & 8 != 0
  - id: comment
    type:
      str:
        encoding: ascii
        terminator: 0
        size: eos
      if: flags & 16 != 0
  - id: hcrc
    type: u2be
    if: flags & 2 != 0
  - id: compressed
    type: bytes
    size: eos
  - id: isize
    type: u4be