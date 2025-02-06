gzip_file {
  id1: U8 = 0x1f;
  id2: U8 = 0x8b;
  cm: U8 = 0x08;
  flg: U8;
  mtime: U32;
  xfl: U8;
  os: U8;
  xlen: U16 if flg & 0x04;
  extra_field: Array(xlen, U8) if flg & 0x04;
  fname: NullTerminatedString if flg & 0x08;
  fcomment: NullTerminatedString if flg & 0x10;
  crc16: U16 if flg & 0x02;
  compressed_data: Array(.., U8);
  crc32: U32;
  isize: U32;
}