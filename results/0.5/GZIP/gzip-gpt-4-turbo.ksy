meta:
  id: gzip
  title: Gzip (GNU zip) archive format
  file-extension: gz
  endian: le
  license: GPL-2.0+
doc: |
  Gzip is a popular and standard single-file archiving format. It essentially
  wraps a deflate-compressed payload with a simple header and footer. Gzip is
  widely used on Unix operation systems for file compression.
doc-ref: https://tools.ietf.org/html/rfc1952
seq:
  - id: header
    type: header
  - id: body
    size-eos: true
  - id: footer
    type: footer
types:
  header:
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
        encoding: UTF-8
        if: flags.has_name
      - id: comment
        type: strz
        encoding: UTF-8
        if: flags.has_comment
      - id: hcrc
        type: u2
        if: flags.has_crc
  flags:
    seq:
      - id: reserved
        type: b3
      - id: has_crc
        type: b1
      - id: has_extra
        type: b1
      - id: has_name
        type: b1
      - id: has_comment
        type: b1
  extras:
    seq:
      - id: len
        type: u2
      - id: data
        size: len
  footer:
    seq:
      - id: crc32
        type: u4
      - id: input_size
        type: u4