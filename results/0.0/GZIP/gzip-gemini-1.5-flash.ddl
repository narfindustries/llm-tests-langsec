gzip = record {
  id1: uint8 := 0x1f,
  id2: uint8 := 0x8b,
  cm: uint8 := 8,
  flg: uint8,
  mtime: uint32,
  xfl: uint8,
  os: uint8,
  extra: optional (record {
    id: uint16,
    len: uint16,
    data: bytes : len
  }),
  fname: optional (cstring),
  fcomment: optional (cstring),
  fhcrc: optional (uint16),
  compressed_data: bytes,
  crc32: uint32,
  isize: uint32
}
