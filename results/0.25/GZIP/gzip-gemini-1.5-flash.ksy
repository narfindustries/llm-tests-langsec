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
  - id: extra
    type: seq
    repeat: expr
    expr: this.flg.fextra
    seq:
      - id: subfield_id
        type: u2le
      - id: subfield_len
        type: u2le
      - id: subfield_data
        type: bytes
        size: this.subfield_len
  - id: fname
    type: str
    term: 0
    repeat: expr
    expr: this.flg.fname
  - id: fcomment
    type: str
    term: 0
    repeat: expr
    expr: this.flg.fcomment
  - id: fhcrc
    type: u2le
    repeat: expr
    expr: this.flg.fhcrc
  - id: compressed_data
    type: bytes
  - id: crc32
    type: u4le
  - id: isize
    type: u4le

