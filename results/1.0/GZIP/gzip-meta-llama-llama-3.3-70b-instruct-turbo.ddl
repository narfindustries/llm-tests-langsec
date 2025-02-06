type gzip {
  id1: uint8 = 0x1F;
  id2: uint8 = 0x8B;
  cm: uint8 = 8;
  flags: bits<8> {
    ftext: 0;
    fhdr_crc: 1;
    reserved: 2..7;
  }
  mtime: uint32<little>;
  xfl: uint8;
  os: uint8;
  xlen: uint16<little>;
  extra: bytes[xlen] if xlen != 0;
  fname: string if (flags & 0x01) != 0;
  fhdr_crc: uint16<little> if (flags & 0x02) != 0;
  compressed: bytes until eof;
  isize: uint32<little>;
}