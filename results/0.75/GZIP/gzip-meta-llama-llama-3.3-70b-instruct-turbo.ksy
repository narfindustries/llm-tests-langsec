meta:
  id: gzip_meta
  file-extension: gz
  fields:
    - name: compression_method
      type: u1
    - name: flags
      type: u1
    - name: mtime
      type: u4le
    - name: extra_flags
      type: u1
    - name: os
      type: u1
  imports:
    - id: gzip_extra
      path: /gzip-extra.ksy

seq:
  - id: compression_method
    type: u1
  - id: flags
    type: u1
  - id: mtime
    type: u4le
  - id: extra_flags
    type: u1
  - id: os
    type: u1
  - id: extra
    type: gzip_extra
    size: (flags & 0x04) != 0
  - id: filename
    type: str
    size: (flags & 0x08) != 0
    encoding: ASCII
  - id: comment
    type: str
    size: (flags & 0x10) != 0
    encoding: ASCII
  - id: crc16
    type: u2le
    size: (flags & 0x02) != 0