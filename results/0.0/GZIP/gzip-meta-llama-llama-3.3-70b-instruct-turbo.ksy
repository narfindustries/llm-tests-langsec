meta:
  id: gzip
  file-extension: gz
  endian: le
seq:
  - id: magic
    contents: [31, 139]
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
  - id: extra
    seq:
      - id: extra_len
        type: u2
      - id: extra_data
        size: extra_len
      - id: extra_subfield
        type: extra_subfield
        repeat: expr
        repeat-expr: extra_len > 0
  - id: filename
    type: str
    terminator: 0
  - id: comment
    type: str
    terminator: 0
  - id: crc16
    type: u2
  - id: body
    process: xor
    process-xor: 31
    size: eos
types:
  extra_subfield:
    seq:
      - id: id
        type: u2
      - id: len
        type: u2
      - id: data
        size: len