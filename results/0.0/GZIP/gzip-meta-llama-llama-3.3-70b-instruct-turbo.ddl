format gzip_file {
  id1: byte = 0x1F;
  id2: byte = 0x8B;
  cm: byte = 8;
  flags: byte;
  mtime: uint32;
  xfl: byte;
  os: byte;
  xlen: uint16 = if (flags & 0x04 != 0);
  extra: bytes[xlen] = if (flags & 0x04 != 0);
  fname: string = if (flags & 0x08 != 0);
  fcomment: string = if (flags & 0x10 != 0);
  hcrc: uint16 = if (flags & 0x02 != 0);
  comprdata: bytes[*];
  isize: uint32;
}