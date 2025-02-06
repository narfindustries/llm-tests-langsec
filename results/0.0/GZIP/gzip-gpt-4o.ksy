meta:
  id: gzip
  title: GZIP
  file-extension: gz
  xref:
    rfc: 1952
  license: CC0-1.0
  endian: le

seq:
  - id: id1
    type: u1
    valid: 0x1F
  - id: id2
    type: u1
    valid: 0x8B
  - id: compression_method
    type: u1
    valid: 0x08
  - id: flags
    type: flags
  - id: mtime
    type: u4
  - id: extra_flags
    type: u1
  - id: os
    type: u1
  - id: extra
    type: extra_field
    if: flags.fextra
  - id: original_file_name
    type: strz
    encoding: UTF-8
    if: flags.fname
  - id: file_comment
    type: strz
    encoding: UTF-8
    if: flags.fcomment
  - id: header_crc16
    type: u2
    if: flags.fhcrc
  - id: compressed_data
    size: _io.size - _io.pos - 8
  - id: crc32
    type: u4
  - id: isize
    type: u4

types:
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

  extra_field:
    seq:
      - id: subfields
        repeat: eos
        type: subfield

  subfield:
    seq:
      - id: id
        type: u2
      - id: len_data
        type: u2
      - id: data
        size: len_data