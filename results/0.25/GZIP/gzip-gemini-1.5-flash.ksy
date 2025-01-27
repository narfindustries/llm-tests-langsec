meta:
  id: gzip-gemini-1
  title: GZIP Gemini 1.5 Flash
  endian: be

seq:
  - id: magic
    type: u4
    enum:
      0x1f8b0800: gzip
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
  - id: fhcrc
    type: u2
    doc: Header CRC
  - id: namelen
    type: u2
    doc: Filename length
  - id: filename
    type: str
    size: namelen
    doc: Filename
  - id: commentlen
    type: u2
    doc: Comment length
  - id: comment
    type: str
    size: commentlen
    doc: Comment
  - id: hcrc
    type: u2
    doc: Header CRC
  - id: compressed_data
    type: u1
    size: (this.body_len)
  - id: crc32
    type: u4
    doc: CRC32 checksum
  - id: isize
    type: u4
    doc: Uncompressed size
  
types:
  body_len:
    expr: (this._root.size - this._io.pos()) - 8
