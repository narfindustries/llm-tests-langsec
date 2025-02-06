meta:
  id: gzip
  title: Gzip Archive
  file-extension: gz
  endian: le
  license: CC0-1.0
doc: |
  Gzip is a file format used for file compression and decompression. The format
  is defined in RFC 1952. This specication is designed to be sufficient to
  decompress any Gzip file conforming to the RFC 1952 specification.
seq:
  - id: magic
    contents: [0x1f, 0x8b]
  - id: compression_method
    contents: [0x08]
  - id: flags
    type: flags
  - id: mod_time
    type: u4
  - id: extra_flags
    type: u1
  - id: os
    type: u1
  - id: extras
    type: extras
    if: flags.has_extra
  - id: name
    type: strz
    encoding: ASCII
    if: flags.has_name
  - id: comment
    type: strz
    encoding: ASCII
    if: flags.has_comment
  - id: header_crc16
    type: u2
    if: flags.has_header_crc
  - id: body
    size-eos: true
    type: gzip_body
types:
  flags:
    seq:
      - id: text
        type: b1
      - id: has_header_crc
        type: b1
      - id: has_extra
        type: b1
      - id: has_name
        type: b1
      - id: has_comment
        type: b1
      - id: reserved
        type: b3
  extras:
    seq:
      - id: len_extra
        type: u2
      - id: extra_fields
        size: len_extra
  gzip_body:
    seq:
      - id: compressed_data
        size-eos: true
      - id: crc32
        type: u4
      - id: isize
        type: u4