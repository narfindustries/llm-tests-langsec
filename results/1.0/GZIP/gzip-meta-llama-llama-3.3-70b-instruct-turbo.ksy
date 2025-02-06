meta:
  id: gzip
  endian: le
  file-extension: gz
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
    seq:
      - id: xlen
        type: u2
      - id: extra_field
        size: xlen
        type: bytes
    if: flags & 4 != 0
  - id: filename
    seq:
      - id: filename_str
        type: strz
    if: flags & 8 != 0
  - id: comment
    seq:
      - id: comment_str
        type: strz
    if: flags & 16 != 0
  - id: hcrc
    seq:
      - id: hcrc_value
        type: u2
    if: flags & 2 != 0
  - id: compressed_data
    type: bytes
    size: eos
types:
  u1: uint8
  u2: uint16
  u4: uint32
  bytes: byte-array
  strz: strz