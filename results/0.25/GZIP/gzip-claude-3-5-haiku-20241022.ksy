meta:
  id: gzip
  file-extension: gz
  endian: le
seq:
  - id: identification
    contents: [0x1F, 0x8B]
  - id: compression_method
    type: u1
    valid: 8
  - id: flags
    type: u1
  - id: modification_time
    type: u4
  - id: extra_flags
    type: u1
  - id: operating_system
    type: u1
    enum: os_type
  - id: extra_field
    type: extra_field
    if: (flags & 0b00000100) != 0
  - id: original_filename
    type: str
    terminator: 0
    encoding: UTF-8
    if: (flags & 0b00001000) != 0
  - id: file_comment
    type: str
    terminator: 0
    encoding: UTF-8
    if: (flags & 0b00010000) != 0
  - id: header_crc16
    type: u2
    if: (flags & 0b00000010) != 0
  - id: compressed_data
    type: deflate_data
  - id: crc32
    type: u4
  - id: uncompressed_size
    type: u4

types:
  extra_field:
    seq:
      - id: length
        type: u2
      - id: data
        size: length
  deflate_data:
    seq:
      - id: raw_data
        type: byte_array
        size-eos: true

enums:
  os_type:
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