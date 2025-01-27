domain GZIP {
  id: Uint32 be;
  compression_method: Uint8;
  flags: Uint8;
  modification_time: Uint32 be;
  extra_flags: Uint8;
  operating_system: Uint8;

  body: Byte[*];

  extra_field: if (flags & 0x04) {
    id: Uint8[*];
    length: Uint16 be;
    data: Byte[length];
  }

  file_name: if (flags & 0x08) {
    Byte[*] until 0x00;
  }

  comment: if (flags & 0x10) {
    Byte[*] until 0x00;
  }

  header_crc16: if (flags & 0x02) {
    Uint16 be;
  }
}