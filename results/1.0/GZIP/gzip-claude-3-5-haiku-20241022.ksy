meta:
  id: gzip
  file-extension: gz
  endian: le
seq:
  - id: identification1
    type: u1
    valid: 0x1f
  - id: identification2
    type: u1
    valid: 0x8b
  - id: compression_method
    type: u1
    valid: 8
  - id: flags
    type: flags
  - id: modification_time
    type: u4
  - id: extra_flags
    type: u1
  - id: operating_system
    type: u1
  - id: extra_fields
    type: extra_field
    if: flags.extra_field
  - id: original_filename
    type: strz
    if: flags.filename
    encoding: ASCII
  - id: file_comment
    type: strz
    if: flags.file_comment
    encoding: ASCII
  - id: header_crc16
    type: u2
    if: flags.header_crc
  - id: compressed_data
    type: deflate_block
  - id: crc32
    type: u4
  - id: uncompressed_size
    type: u4
types:
  flags:
    seq:
      - id: text
        type: b1
      - id: header_crc
        type: b1
      - id: extra_field
        type: b1
      - id: filename
        type: b1
      - id: file_comment
        type: b1
      - id: reserved
        type: b3
  extra_field:
    seq:
      - id: total_length
        type: u2
      - id: subfields
        type: subfield
        repeat: expr
        repeat-expr: total_length
  subfield:
    seq:
      - id: id
        type: str
        size: 2
        encoding: ASCII
      - id: length
        type: u2
      - id: data
        size: length
  deflate_block:
    seq:
      - id: raw_data
        type: u1
        repeat: eos