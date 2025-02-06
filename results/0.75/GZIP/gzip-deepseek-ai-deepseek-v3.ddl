GZIP {
  ID1: U8 = 0x1f;
  ID2: U8 = 0x8b;
  CM: U8 = 0x08;
  FLG: U8;
  MTIME: U32;
  XFL: U8;
  OS: U8;
  ExtraField: If (FLG & 0x04) {
    XLEN: U16;
    ExtraData: Bytes(XLEN);
  };
  FName: If (FLG & 0x08) {
    Name: CString;
  };
  FComment: If (FLG & 0x10) {
    Comment: CString;
  };
  CRC16: If (FLG & 0x02) {
    CRC: U16;
  };
  CompressedData: Bytes;
  CRC32: U32;
  ISIZE: U32;
}