meta:
  id: gzip_meta
  file-extension: gz
  kaitai-struct-version: 0.9
doc: GZIP file format
seq:
  - id: compression_method
    type: u1
  - id: flags
    type: u1
  - id: os
    type: u1
  - id: mod_time
    type: u4le
  - id: extra_flags
    type: u1
  - id: extra_field
    type:
      proc: if flags & 4 != 0
      type: extra_field
  - id: file_name
    type:
      proc: if flags & 8 != 0
      type: file_name
  - id: comment
    type:
      proc: if flags & 16 != 0
      type: comment
  - id: header_crc16
    type:
      proc: if flags & 2 != 0
      type: u2le

types:
  extra_field:
    seq:
      - id: len
        type: u2le
      - id: data
        type: str
        size: len
        encoding: latin1

  file_name:
    seq:
      - id: data
        type: str
        encoding: latin1
        term: \x00

  comment:
    seq:
      - id: data
        type: str
        encoding: latin1
        term: \x00