meta:
  id: gzip
  title: Gzip Archive
  file-extension: gz
  endian: le
  license: CC0-1.0
doc: |
  Gzip is a file format used for file compression and decompression. The format was introduced by the GNU project in 1992, and is based on the DEFLATE algorithm, which is a combination of LZ77 and Huffman coding.

seq:
  - id: members
    type: member
    repeat: eos

types:
  member:
    seq:
      - id: header
        type: header
      - id: extra
        type: extra_field
        if: header.flags.has_extra
      - id: name
        type: strz
        encoding: UTF-8
        if: header.flags.has_name
      - id: comment
        type: strz
        encoding: UTF-8
        if: header.flags.has_comment
      - id: header_crc16
        size: 2
        if: header.flags.has_header_crc
      - id: body
        size-eos: true
      - id: footer
        type: footer

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

  flags:
    seq:
      - id: reserved
        type: b3
      - id: has_comment
        type: b1
      - id: has_name
        type: b1
      - id: has_extra
        type: b1
      - id: has_header_crc
        type: b1
      - id: is_text
        type: b1

  extra_field:
    seq:
      - id: len_extra
        type: u2
      - id: extra
        size: len_extra

  footer:
    seq:
      - id: crc32
        type: u4
      - id: input_size
        type: u4

  strz:
    seq:
      - id: str
        type: strz
        encoding: UTF-8
        terminator: 0
        include: false
        eos-error: false