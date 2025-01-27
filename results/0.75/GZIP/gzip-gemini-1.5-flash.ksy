meta:
  id: gzip-gemini-1
  endian: be

seq:
  - id: magic
    type: u4
    enum:
      0x1f8b08: gzip
  - id: cm
    type: u2
    enum:
      8: deflate
  - id: flg
    type: u2
  - id: mtime
    type: u4
  - id: xfl
    type: u1
  - id: os
    type: u1
  - id: extra
    type: bytes
    repeat: expr
    expr: (flg & 0x4) != 0 ? this.xlen : 0
  - id: xlen:
    type: u2
    if: (flg & 0x4) != 0
  - id: fname
    type: str
    size: expr
    expr: (flg & 0x8) != 0 ? this.fname_len : 0
    encoding: utf-8
  - id: fname_len:
    type: u1
    if: (flg & 0x8) != 0
  - id: fcomment
    type: str
    size: expr
    expr: (flg & 0x10) != 0 ? this.fcomment_len : 0
    encoding: utf-8
  - id: fcomment_len:
    type: u1
    if: (flg & 0x10) != 0
  - id: crc16
    type: u2
    if: (flg & 0x2) != 0
  - id: compressed_data
    type: bytes
    repeat: expr
    expr: this.isize > 0 ? this.isize : 0
  - id: isize
    type: u4
