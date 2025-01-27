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
    doc: compression method
  - id: flg
    type: u1
    doc: flags
  - id: mtime
    type: u4
    doc: modification time
  - id: xfl
    type: u1
    doc: extra flags
  - id: os
    type: u1
    doc: operating system
  - id: xlen
    type: u2
    doc: extra length
  - id: extra
    type: u2
    len: xlen
  - id: fhcrc
    type: u2
    doc: header crc
  - id: name
    type: str
    len: (flen, fhcrc)
  - id: comment
    type: str
    len: (clen, fhcrc)
  - id: hcrc
    type: u2
    doc: header crc
  - id: compressed_data
    type: u1
    len: (usize - (flen, fhcrc, clen, hcrc))
  - id: crc
    type: u4
  - id: isize
    type: u4
    doc: uncompressed size
