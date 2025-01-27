meta:
  id: gzip
  file-extension: gz
  endian: le
seq:
  - id: magic
    type: bytes
    size: 2
    contents: [0x1f, 0x8b]
  - id: compression_method
    type: u1
  - id: flags
    type: u1
  - id: mtime
    type: u4
  - id: extra_flags
    type: u1
  - id: os
    type: u1
  - id: extra
    type:
      proc: if flags & 4 != 0
      seq:
        - id: xlen
          type: u2
        - id: extra_data
          size: xlen
          type: bytes
  - id: filename
    type:
      proc: if flags & 8 != 0
      seq:
        - id: filename_len
          type: u1
        - id: filename_data
          size: filename_len
          type: str
          encoding: ascii
  - id: comment
    type:
      proc: if flags & 16 != 0
      seq:
        - id: comment_len
          type: u1
        - id: comment_data
          size: comment_len
          type: str
          encoding: ascii
  - id: crc16
    type:
      proc: if flags & 2 != 0
      type: u2
  - id: body
    type: switch-on compression_method
    cases:
      8: seq
          - id: compressed_data
            size: eos
            type: bytes