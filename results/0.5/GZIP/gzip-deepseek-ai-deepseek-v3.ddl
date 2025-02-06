gzip_file: sequence
  header: sequence
    id1: uint8 = 0x1f
    id2: uint8 = 0x8b
    cm: uint8 = 0x08
    flg: uint8
    mtime: uint32
    xfl: uint8
    os: uint8
    extra: optional sequence
      xlen: uint16
      data: bytes(size=xlen)
    name: optional null_terminated_string if (flg & 0x08) != 0
    comment: optional null_terminated_string if (flg & 0x10) != 0
    crc16: optional uint16 if (flg & 0x02) != 0
  compressed_blocks: bytes
  footer: sequence
    crc32: uint32
    isize: uint32