meta:
  id: gzip
  file-extension: gz
  endian: le
seq:
  - id: identification
    contents: [0x1F, 0x8B]
  - id: compression_method
    type: u1
    enum: compression_methods
  - id: flags
    type: flags_type
  - id: modification_time
    type: u4
  - id: extra_flags
    type: u1
    enum: compression_levels
  - id: operating_system
    type: u1
    enum: operating_systems
  - id: extra_field
    type: extra_field
    if: flags.extra_fields
  - id: original_filename
    type: strz
    encoding: ASCII
    if: flags.original_filename
  - id: file_comment
    type: strz
    encoding: ASCII
    if: flags.file_comment
  - id: header_crc
    type: u2
    if: flags.header_crc
  - id: compressed_data
    type: deflate_block
  - id: crc32
    type: u4
  - id: uncompressed_size
    type: u4

types:
  flags_type:
    seq:
      - id: text_hint
        type: b1
      - id: header_crc
        type: b1
      - id: extra_fields
        type: b1
      - id: original_filename
        type: b1
      - id: file_comment
        type: b1
      - id: encrypted
        type: b1
      - id: reserved
        type: b2

  extra_field:
    seq:
      - id: length
        type: u2
      - id: data
        size: length

  deflate_block:
    seq:
      - id: raw_data
        type: str
        encoding: ASCII
        size-eos: true

enums:
  compression_methods:
    8: deflate

  compression_levels:
    2: max_compression
    4: fastest_algorithm

  operating_systems:
    0: fat
    1: amiga
    2: vms
    3: unix
    4: vm_cms
    5: atari
    6: hpfs
    7: macintosh
    8: z_system
    9: cp_m
    10: tops_20
    11: ntfs
    12: qdos
    13: acorn_riscos
    255: unknown