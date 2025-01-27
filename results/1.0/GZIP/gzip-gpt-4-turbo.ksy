meta:
  id: gzip
  title: GZIP Archive
  file-extension: gz
  endian: le
  license: CC0-1.0
  ks-version: 0.9

doc: |
  GZIP is a file format for file compression and decompression.
  GZIP format is a Lempel-Ziv coding (LZ77) with a 32-bit CRC.

seq:
  - id: header
    type: header
  - id: body
    size-eos: true
  - id: isize
    type: u4

types:
  header:
    seq:
      - id: magic
        contents: [0x1f, 0x8b]
      - id: compression_method
        type: u1
        valid: 8  # Deflate
      - id: flags
        type: flags
      - id: mtime
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

    types:
      flags:
        seq:
          - id: has_extra
            type: b1
          - id: has_name
            type: b1
          - id: has_comment
            type: b1
          - id: has_crc
            type: b1
          - id: reserved
            type: b4

      extras:
        seq:
          - id: len
            type: u2
          - id: data
            size: len
