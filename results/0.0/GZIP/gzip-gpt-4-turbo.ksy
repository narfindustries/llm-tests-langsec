meta:
  id: gzip
  title: Gzip (GNU zip) archive format
  file-extension: gz
  endian: le
  license: GPL-2.0+
  ks-version: 0.9
doc: |
  Gzip is a popular file compression algorithm that is based on the DEFLATE algorithm, which is a combination of LZ77 and Huffman coding. It is used to compress files, often as a final step before transmission over a network.
doc-ref: https://tools.ietf.org/html/rfc1952
seq:
  - id: magic
    contents: [0x1f, 0x8b]
  - id: compression_method
    type: u1
    enum: compression_methods
    valid:
      eq: compression_methods.deflate
  - id: flags
    type: flags
  - id: mod_time
    type: u4
  - id: extra_flags
    type: u1
  - id: os
    type: u1
    enum: operating_systems
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
  - id: header_crc16
    type: u2
    if: flags.has_header_crc
  - id: body
    size-eos: true
    type: deflate_blocks
  - id: crc32
    type: u4
  - id: uncompressed_size
    type: u4
types:
  flags:
    seq:
      - id: reserved
        type: b3
      - id: has_header_crc
        type: b1
      - id: has_extra
        type: b1
      - id: has_name
        type: b1
      - id: has_comment
        type: b1
      - id: is_text
        type: b1
  extras:
    seq:
      - id: len_extra
        type: u2
      - id: extras
        type: extra_field
        repeat: expr
        repeat-expr: len_extra
  extra_field:
    seq:
      - id: si1
        type: u1
      - id: si2
        type: u1
      - id: len_field
        type: u2
      - id: data
        size: len_field
  deflate_blocks:
    seq:
      - id: blocks
        type: deflate_block
        repeat: eos
  deflate_block:
    seq:
      - id: last_block
        type: b1
      - id: block_type
        type: b2
      - id: data
        type:
          switch-on: block_type
          cases:
            0: stored_block
            1: fixed_huffman_block
            2: dynamic_huffman_block
  stored_block:
    seq:
      - id: len_block
        type: u2
      - id: nlen_block
        type: u2
      - id: data
        size: len_block
  fixed_huffman_block:
    # Implementation-specific details would go here
  dynamic_huffman_block:
    # Implementation-specific details would go here
enums:
  compression_methods:
    8: deflate
  operating_systems:
    0: fat
    1: amiga
    2: vms
    3: unix
    4: vm_cms
    5: atari_tos
    6: hpfs
    7: macintosh
    8: z_system
    9: cp_m
    10: tops_20
    11: ntfs
    12: qdos
    13: acorn_riscos
    255: unknown