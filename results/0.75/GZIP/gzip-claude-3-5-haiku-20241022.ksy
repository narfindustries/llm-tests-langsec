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
  - id: mtime
    type: u4
    doc: Modification time in Unix timestamp format
  - id: extra_flags
    type: u1
    enum: extra_flags_enum
  - id: os
    type: u1
    enum: os_enum
  - id: extra_field
    type: extra_field
    if: flags.fextra
  - id: filename
    type: strz
    encoding: UTF-8
    if: flags.fname
  - id: comment
    type: strz
    encoding: UTF-8
    if: flags.fcomment
  - id: header_crc16
    type: u2
    if: flags.fhcrc
  - id: compressed_data
    type: compressed_block
  - id: crc32
    type: u4
  - id: uncompressed_size
    type: u4
types:
  flags_type:
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
      - id: reserved1
        type: b1
      - id: reserved2
        type: b1
      - id: reserved3
        type: b1
  extra_field:
    seq:
      - id: extra_len
        type: u2
      - id: extra_data
        size: extra_len
  compressed_block:
    seq:
      - id: data
        type: byte_array
  byte_array:
    seq:
      - id: value
        type: u1
        repeat: eos
enums:
  compression_methods:
    8: deflate
  extra_flags_enum:
    2: max_compression
    4: fastest_compression
  os_enum:
    0: fat
    3: unix
    7: macintosh
    11: ntfs
    255: unknown