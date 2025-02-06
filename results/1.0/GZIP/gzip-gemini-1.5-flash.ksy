type: seq
- id: magic
  type: bits
  size: 16
  - id: id1
    type: u1
  - id: id2
    type: u1
- id: cm
  type: u1
- id: flg
  type: u1
- id: mtime
  type: u4be
- id: xfl
  type: u1
- id: os
  type: u1
- id: fextra
  type: seq
  condition: flg.fextra
  - id: size
    type: u2be
  - id: data
    type: bytes
    size: size
- id: fname
  type: strz
  condition: flg.fname
- id: fcomment
  type: strz
  condition: flg.fcomment
- id: fhcrc
  type: u2be
  condition: flg.fhcrc & flg.fextra
- id: compressed_data
  type: bytes
  size: compressed_data.length
- id: crc32
  type: u4be
- id: isize
  type: u4be
