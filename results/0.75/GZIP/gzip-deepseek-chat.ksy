meta:
  id: gzip_deepseek_chat
  file-extension: gz
  endian: le
  encoding: UTF-8
seq:
  - id: magic
    contents: [0x1f, 0x8b]
  - id: compression_method
    type: u1
    enum: compression_methods
  - id: flags
    type: u1
    enum: flags
  - id: modification_time
    type: u4
  - id: extra_flags
    type: u1
    enum: extra_flags
  - id: os
    type: u1
    enum: os_types
  - id: extra_field
    type: extra_field
    if: flags.fextra
  - id: original_filename
    type: strz
    encoding: UTF-8
    if: flags.fname
  - id: comment
    type: strz
    encoding: UTF-8
    if: flags.fcomment
  - id: header_crc
    type: u2
    if: flags.fhcrc
  - id: compressed_data
    type: compressed_data
  - id: crc32
    type: u4
  - id: uncompressed_size
    type: u4
enums:
  compression_methods:
    8: deflate
  flags:
    0x01: ftext
    0x02: fhcrc
    0x04: fextra
    0x08: fname
    0x10: fcomment
  extra_flags:
    2: max_compression
    4: fast_compression
  os_types:
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
    13: acorn_risc
    255: unknown
types:
  extra_field:
    seq:
      - id: len
        type: u2
      - id: data
        size: len
  compressed_data:
    seq:
      - id: blocks
        type: deflate_block
        repeat: eos
  deflate_block:
    seq:
      - id: bfinal
        type: b1
      - id: btype
        type: b2
        enum: block_types
      - id: data
        type:
          switch-on: btype
          cases:
            0: uncompressed_block
            1: fixed_huffman_block
            2: dynamic_huffman_block
  uncompressed_block:
    seq:
      - id: len
        type: u2
      - id: nlen
        type: u2
      - id: data
        size: len
  fixed_huffman_block:
    seq:
      - id: data
        type: huffman_data
  dynamic_huffman_block:
    seq:
      - id: hlit
        type: b5
      - id: hdist
        type: b5
      - id: hclen
        type: b4
      - id: code_lengths
        type: code_length
        repeat: expr
        repeat-expr: hclen + 4
      - id: data
        type: huffman_data
  code_length:
    seq:
      - id: length
        type: b3
  huffman_data:
    seq:
      - id: data
        type: u1
        repeat: eos
enums:
  block_types:
    0: uncompressed
    1: fixed_huffman
    2: dynamic_huffman