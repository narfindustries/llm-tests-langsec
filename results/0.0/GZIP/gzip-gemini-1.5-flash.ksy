meta:
  id: gzip-gemini-1
  title: GZIP Gemini 1.5 Flash
  homepage: https://kaitai.io
types:
  gzip_header:
    seq:
      - id: magic
        type: u4
        enum:
          - 0x1f8b: gzip
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
  gzip_extra:
    seq:
      - id: size
        type: u2
      - id: data
        type: u2
        size: size
  gzip_footer:
    seq:
      - id: crc32
        type: u4
      - id: isize
        type: u4
  gzip_block:
    seq:
      - id: btype
        type: u1
        enum:
          - 0: uncompressed
          - 1: compressed
          - 2: compressed
          - 3: reserved
      - id: data:
        type: u1
        size: if btype == 0 then 0 else 1024
  gzip_file:
    seq:
      - id: header
        type: gzip_header
      - id: extra
        type: gzip_extra
        if: header.flg & 0x04
      - id: name
        type: str
        size: lambda: header.flg & 0x08
      - id: comment
        type: str
        size: lambda: header.flg & 0x10
      - id: hcrc
        type: u2
        if: header.flg & 0x02
      - id: blocks
        type: gzip_block+
      - id: footer
        type: gzip_footer

