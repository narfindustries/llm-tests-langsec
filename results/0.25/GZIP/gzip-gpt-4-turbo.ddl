GzipFile := struct {
  id1: uint8 = 0x1f; // ID1, fixed value 0x1f
  id2: uint8 = 0x8b; // ID2, fixed value 0x8b
  cm: uint8;  // Compression Method, 8 = deflate
  flg: uint8; // Flags
  mtime: uint32; // Modification time
  xfl: uint8;  // Extra flags
  os: uint8;   // Operating system

  fextra: ExtraField if (flg & 0x04) != 0;
  fname: CString if (flg & 0x08) != 0;
  fcomment: CString if (flg & 0x10) != 0;
  fhcrc: uint16 if (flg & 0x02) != 0;

  compressed_data: bytes;
  crc32: uint32; // CRC-32 checksum
  isize: uint32;  // Input size
};

ExtraField := struct {
  xlen: uint16;
  xdata: bytes[xlen];
};

CString := struct {
  data: bytes until \0;
};