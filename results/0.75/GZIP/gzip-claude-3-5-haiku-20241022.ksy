meta:
  id: gzip
  file-extension: gz
  endian: le
seq:
  - id: header
    type: header
  - id: compressed_data
    size-eos: true
types:
  header:
    seq:
      - id: magic
        contents: [0x1f, 0x8b]
      - id: compression_method
        type: u1
        enum: compression_methods
      - id: flags
        type: flags
      - id: modification_time
        type: u4
      - id: extra_flags
        type: u1
      - id: operating_system
        type: u1
        enum: operating_systems
      - id: optional_extra_fields
        type: optional_extra_fields
        if: flags.fextra
      - id: optional_filename
        type: str
        encoding: UTF-8
        terminator: 0x00
        if: flags.fname
      - id: optional_comment
        type: str
        encoding: UTF-8
        terminator: 0x00
        if: flags.fcomment
      - id: optional_header_crc16
        type: u2
        if: flags.fhcrc
  flags:
    seq:
      - id: ftext
        type: b1
      - id: fhcrc
        type: b1
      - id: fextra
        type: b1
      - id: fname
        type: b1
      - id: fcomment
        type: b1
      - id: reserved
        type: b3
  optional_extra_fields:
    seq:
      - id: extra_length
        type: u2
      - id: extra_data
        size: extra_length
  footer:
    seq:
      - id: crc32
        type: u4
      - id: uncompressed_size
        type: u4
enums:
  compression_methods:
    8: deflate
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
    10: tops20
    11: ntfs
    12: qdos
    13: acorn
    14: vfat
    15: mvs
    16: beos
    17: tandem
    18: theos
    255: unknown