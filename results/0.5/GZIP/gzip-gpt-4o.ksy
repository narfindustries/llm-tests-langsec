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
  - id: compressed_data
    size-eos: true
    type: u1
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
        type: extra_subfields
        if: (flags.has_extra_subfields == true)
      - id: name
        type: strz
        encoding: UTF-8
        if: (flags.has_filename == true)
      - id: comment
        type: strz
        encoding: UTF-8
        if: (flags.has_comment == true)
      - id: header_crc16
        type: u2
        if: (flags.has_header_crc == true)

  extra_subfields:
    seq:
      - id: subfield_id
        type: u2
      - id: subfield_length
        type: u2
      - id: subfield_data
        size: subfield_length

enums:
  compression_methods:
    deflate: 8

  flags:
    has_extra_subfields: 4
    has_filename: 8
    has_comment: 16
    has_header_crc: 2