format gzip {
  id1: uint8 = 0x1f;
  id2: uint8 = 0x8b;
  cm: uint8 = 8;
  flags: uint8;
  mtime: uint32;
  xfl: uint8;
  os: uint8;

  if (flags & 4) != 0 {
    xlen: uint16;
    extra: bytes[xlen];
  }

  if (flags & 8) != 0 {
    fname: string0;
  }

  if (flags & 16) != 0 {
    fcomment: string0;
  }

  if (flags & 2) != 0 {
    fhdr_crc: uint16;
  }

  compressed: bytes[*];
  crc32: uint32;
  isize: uint32;
}