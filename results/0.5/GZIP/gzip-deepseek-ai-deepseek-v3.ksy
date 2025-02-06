meta:
  id: gzip
  file-extension: gz
  endian: le
seq:
  - id: header
    type: header
  - id: extra_fields
    type: extra_fields
    if: header.flags.fextra
  - id: filename
    type: strz
    if: header.flags.fname
    encoding: UTF-8
  - id: comment
    type: strz
    if: header.flags.fcomment
    encoding: UTF-8
  - id: header_crc16
    type: u2
    if: header.flags.fhcrc
  - id: compressed_data
    size-eos: true
    type: deflate
  - id: trailer
    type: trailer
types:
  header:
    seq:
      - id: id1
        type: u1
        valid: 0x1F
      - id: id2
        type: u1
        valid: 0x8B
      - id: cm
        type: u1
        valid: 8
      - id: flags
        type: flags
      - id: mtime
        type: u4
      - id: xfl
        type: u1
      - id: os
        type: u1
  flags:
    seq:
      - id: ftext
        type: b1
      - id: fhcrc
        type: b1
      - id: fextra
        type: b1
      - id: fname
        type: b1
      - id: fcomment
        type: b1
      - id: reserved
        type: b3
  extra_fields:
    seq:
      - id: xlen
        type: u2
      - id: data
        size: xlen
  trailer:
    seq:
      - id: crc32
        type: u4
      - id: isize
        type: u4
  deflate:
    seq:
      - id: data
        size-eos: true
  strz:
    seq:
      - id: value
        type: str
        encoding: UTF-8
        terminator: 0