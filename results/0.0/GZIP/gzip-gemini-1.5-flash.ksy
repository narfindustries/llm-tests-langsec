type: struct
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
      8: deflate
  - id: flg
    type: u1
    bits:
      - ftext: 1
      - fhcrc: 1
      - fextra: 1
      - fname: 1
      - fcomment: 1
      - reserved: 3
  - id: mtime
    type: u4le
  - id: xfl
    type: u1
  - id: os
    type: u1
  - id: fextra
    type: seq
    if: flg.fextra
    - id: size
      type: u2le
    - id: data
      type: bytes
      size: size
  - id: fname
    type: strz
    if: flg.fname
  - id: fcomment
    type: strz
    if: flg.fcomment
  - id: fhcrc
    type: u2le
    if: flg.fhcrc
  - id: compressed_data
    type: bytes
  - id: crc32
    type: u4le
  - id: isize
    type: u4le

