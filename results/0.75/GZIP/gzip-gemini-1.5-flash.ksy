type: seq
endian: be
seq:
  - id: id1
    type: u1
    enum:
      0x1f: id1
  - id: id2
    type: u1
    enum:
      0x8b: id2
  - id: cm
    type: u1
    enum:
      0x08: deflate
  - id: flg
    type: u1
  - id: mtime
    type: u4le
  - id: xfl
    type: u1
  - id: os
    type: u1
  - id: xlen
    type: u2le
    if: (flg & 0x04) != 0
  - id: extra
    type: bytes
    size: xlen
    if: (flg & 0x04) != 0
  - id: fname
    type: str
    encoding: UTF-8
    term: 0
    if: (flg & 0x08) != 0
  - id: fcomment
    type: str
    encoding: UTF-8
    term: 0
    if: (flg & 0x10) != 0
  - id: crc16
    type: u2le
    if: (flg & 0x02) != 0
  - id: compressed_data
    type: bytes
  - id: crc32
    type: u4le
  - id: isize
    type: u4le
