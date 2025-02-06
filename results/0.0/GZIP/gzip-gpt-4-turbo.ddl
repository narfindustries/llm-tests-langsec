type GzipFile = struct {
  id1: uint8 = 0x1f,
  id2: uint8 = 0x8b,
  cm: uint8,
  flg: Flags,
  mtime: uint32,
  xfl: uint8,
  os: uint8,
  extra: optional[ExtraField] if flg.fextra == 1,
  fname: optional[cstring] if flg.fname == 1,
  fcomment: optional[cstring] if flg.fcomment == 1,
  fhcrc: optional[uint16] if flg.fhcrc == 1,
  data: bytes,
  crc32: uint32,
  isize: uint32
};

type Flags = struct {
  ftext: uint1,
  fhcrc: uint1,
  fextra: uint1,
  fname: uint1,
  fcomment: uint1,
  reserved: uint3 = 0
};

type ExtraField = struct {
  xlen: uint16,
  xdata: bytes
};