meta:
  id: gzip
  file-extension: gz
  endian: le
seq:
  - id: id1
    type: u1
    valid: 0x1f
  - id: id2
    type: u1
    valid: 0x8b
  - id: cm
    type: u1
    valid: 0x08
  - id: flg
    type: u1
  - id: mtime
    type: u4
  - id: xfl
    type: u1
  - id: os
    type: u1
    enum: os_types
  - id: extra_field
    type: extra_field
    if: '(flg & 0x04) != 0'
  - id: original_filename
    type: strz
    if: '(flg & 0x08) != 0'
    encoding: UTF-8
  - id: comment
    type: strz
    if: '(flg & 0x10) != 0'
    encoding: UTF-8
  - id: header_crc16
    type: u2
    if: '(flg & 0x02) != 0'
  - id: compressed_data
    size-eos: true
    type: deflate
  - id: crc32
    type: u4
  - id: isize
    type: u4
enums:
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
    13: acorn_riscos
    255: unknown
types:
  extra_field:
    seq:
      - id: xlen
        type: u2
      - id: sub_fields
        size: xlen
        type: str
        encoding: UTF-8
  deflate:
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
        valid: '~len'
      - id: data
        size: len
        type: str
        encoding: UTF-8
  fixed_huffman_block:
    seq:
      - id: data
        type: str
        size-eos: true
        encoding: UTF-8
  dynamic_huffman_block:
    seq:
      - id: hlit
        type: u1
      - id: hdist
        type: u1
      - id: hclen
        type: u1
      - id: code_lengths
        type: u1
        repeat: expr
        repeat-expr: hclen + 4
      - id: literal_length_codes
        type: huffman_code
        repeat: expr
        repeat-expr: hlit + 257
      - id: distance_codes
        type: huffman_code
        repeat: expr
        repeat-expr: hdist + 1
      - id: data
        type: str
        size-eos: true
        encoding: UTF-8
  huffman_code:
    seq:
      - id: code_length
        type: u1