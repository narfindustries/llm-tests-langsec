meta:
  id: gzip
  title: GZIP
  file-extension: gz
  xref:
    rfc: 1952
  endian: le

seq:
  - id: header
    type: header
  - id: compressed_data
    size-eos: true
    type: bytes
  - id: crc32
    type: u4
  - id: isize
    type: u4

types:
  header:
    seq:
      - id: id1
        type: u1
        valid: 0x1f
      - id: id2
        type: u1
        valid: 0x8b
      - id: compression_method
        type: u1
        valid: 8
      - id: flags
        type: u1
        enum: flags
      - id: mtime
        type: u4
      - id: extra_flags
        type: u1
      - id: os
        type: u1
      - id: extra_len
        type: u2
        if: '(header.flags.has_extra == true)'
      - id: extra
        size: 'extra_len'
        if: '(header.flags.has_extra == true)'
      - id: name
        type: strz
        encoding: UTF-8
        if: '(header.flags.has_name == true)'
      - id: comment
        type: strz
        encoding: UTF-8
        if: '(header.flags.has_comment == true)'
      - id: header_crc16
        type: u2
        if: '(header.flags.has_header_crc == true)'

  flags:
    seq:
      - id: reserved1
        type: b1
      - id: reserved2
        type: b1
      - id: reserved3
        type: b1
      - id: reserved4
        type: b1
      - id: has_extra
        type: b1
      - id: has_name
        type: b1
      - id: has_comment
        type: b1
      - id: has_header_crc
        type: b1