meta:
  id: gzip-gemini-1
  endian: be

seq:
  - id: magic
    type: u4
    enum:
      0x1f8b: gzip
  - id: cm
    type: u2
    doc: Compression method
  - id: flg
    type: u2
    doc: Flags
  - id: mtime
    type: u4
    doc: Modification time
  - id: xfl
    type: u1
    doc: Extra flags
  - id: os
    type: u1
    doc: Operating system
  - id: xlen
    type: u2
    doc: Extra field length
  - id: extra
    type: u1
    size: xlen
    doc: Extra field
  - id: flen
    type: u2
    doc: Filename length
  - id: filename
    type: str
    size: flen
    doc: Filename
  - id: csum
    type: u2
    doc: Comment length
  - id: comment
    type: str
    size: csum
    doc: Comment
  - id: hcrc
    type: u2
    doc: Header CRC
  - id: compressed_data
    type: u1
    size: is_compressed_data_size
  - id: is_compressed_data_size:
    type: expr
    expr: self.size - self.pos - 8
    doc: Size of compressed data

