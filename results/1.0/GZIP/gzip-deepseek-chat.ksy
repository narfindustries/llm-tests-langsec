meta:
  id: gzip_deepseek_chat
  file-extension: gz
  endian: le
seq:
  - id: magic
    contents: [0x1f, 0x8b]
  - id: compression_method
    type: u1
  - id: flags
    type: u1
  - id: modification_time
    type: u4
  - id: extra_flags
    type: u1
  - id: os
    type: u1
  - id: extra_field
    type: extra_field
    if: flags & 0x04 != 0
  - id: original_filename
    type: strz
    encoding: UTF-8
    if: flags & 0x08 != 0
  - id: comment
    type: strz
    encoding: UTF-8
    if: flags & 0x10 != 0
  - id: header_crc
    type: u2
    if: flags & 0x02 != 0
  - id: compressed_data
    type: compressed_data
  - id: crc32
    type: u4
  - id: uncompressed_size
    type: u4
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
        type: block
        repeat: eos
  block:
    seq:
      - id: is_final
        type: b1
      - id: type
        type: u2
      - id: len
        type: u2
      - id: data
        size: len