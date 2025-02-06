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
  - id: flg
    type: u1
  - id: mtime
    type: u4le
  - id: xfl
    type: u1
  - id: os
    type: u1
  - id: extra_len
    type: u2le
    if: (flg & 0x04) != 0
  - id: extra
    type: bytes
    size: extra_len
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
  - id: fhcrc
    type: u2le
    if: (flg & 0x02) != 0
  - id: compressed_data
    type: bytes
  - id: crc32
    type: u4le
  - id: isize
    type: u4le

