meta:
  id: gzip
  endian: le
seq:
- id: header
  type: header
- id: extra
  type: extra
  if: header.flags.fextra
- id: name
  type: strz
  encoding: ASCII
  if: header.flags.fname
- id: comment
  type: strz
  encoding: ASCII
  if: header.flags.fcomment
- id: hcrc
  type: hcrc
  if: header.flags.fhcrc
- id: body
  type: bytes
- id: isize
  type: u4
types:
  header:
    seq:
    - id: id1
      type: u1
    - id: id2
      type: u1
    - id: cm
      type: u1
    - id: flags
      type: flags
    - id: mtime
      type: u4
    - id: xfl
      type: u1
    - id: os
      type: u1
    - id: validity_check
      type: validity_check
  extra:
    seq:
    - id: len
      type: u2
    - id: data
      type: bytes
      size: len
  hcrc:
    type: u2
  flags:
    type: bits
    bits: 8
    pos:
    - id: ftext
      type: b1
      pos: 0
    - id: fhcrc
      type: b1
      pos: 1
    - id: fextra
      type: b1
      pos: 2
    - id: fname
      type: b1
      pos: 3
    - id: fcomment
      type: b1
      pos: 4
    - id: reserved
      type: b4
      pos: 5
  validity_check:
    if: header.id1 != 31 or header.id2 != 139 or header.cm != 8
    then:
      - id: error
        type: error
        msg: Invalid GZIP header
  error:
    type: str
    value: Invalid GZIP header