meta:
  id: gzip_gpt_4o
  file-extension: gz
  title: Gzip Archive
  application: Gzip file
  xref:
    - https://datatracker.ietf.org/doc/html/rfc1952

doc: |
  This is a specification to parse Gzip compressed files.
  It adheres to RFC 1952.

seq:
  - id: header
    type: header
  - id: compressed_blocks
    type: bytes
    size: header.compressed_size
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
        enum: compression_methods
      - id: flags
        type: u1
      - id: mtime
        type: u4
      - id: extra_flags
        type: u1
      - id: os
        type: u1
      - id: extra
        type: extra_field
        if: (flags & 0x04) != 0
      
  extra_field:
    seq:
      - id: length
        type: u2
      - id: data
        type: bytes
        size: length

enums:
  compression_methods:
    deflate: 8
