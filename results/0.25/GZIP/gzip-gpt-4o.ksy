meta:
  id: gzip
  title: GZIP
  file-extension: gz
  xref:
    rfc: 1952
  license: CC0-1.0
  endian: little
seq:
  - id: header
    type: header
  - id: compressed_blocks
    type: bytes
    size: _io.size - 8
  - id: crc32
    type: u4
  - id: isize
    type: u4
types:
  header:
    seq:
      - id: id1
        contents: [0x1f]
      - id: id2
        contents: [0x8b]
      - id: compression_method
        type: u1
        enum: compression_methods
      - id: flags
        type: u1
        enum: flags
      - id: mtime
        type: u4
      - id: extra_flags
        type: u1
      - id: os
        type: u1
      - id: extra
        type: extra_field
        if: flags.has_extra
      - id: name
        type: strz
        encoding: UTF-8
        if: flags.has_name
      - id: comment
        type: strz
        encoding: UTF-8
        if: flags.has_comment
      - id: header_crc16
        type: u2
        if: flags.has_header_crc
  extra_field:
    seq:
      - id: subfields
        type: subfield
        repeat: eos
  subfield:
    seq:
      - id: id
        type: u2
      - id: len_data
        type: u2
      - id: data
        size: len_data
enums:
  compression_methods:
    deflate: 8
  flags:
    has_extra: 0b00000100
    has_name: 0b00001000
    has_comment: 0b00010000
    has_header_crc: 0b00000010