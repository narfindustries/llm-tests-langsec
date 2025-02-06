gzip = record {
  id1: uint8,
  id2: uint8,
  cm: uint8,
  flg: uint8,
  mtime: uint32,
  xfl: uint8,
  os: uint8,
  extra: optional { length: uint16; data: bytes(length) },
  fname: optional string,
  fcomment: optional string,
  fhcrc: optional uint16,
  compressed_data: bytes,
  crc32: uint32,
  isize: uint32
}
