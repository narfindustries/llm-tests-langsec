domain gzip {
  header gzip_header {
    field magic: uint16 = 0x1f8b;
    field compression_method: uint8 = 8;
    field flags: uint8;
    field modification_time: uint32;
    field extra_flags: uint8;
    field operating_system: uint8;
  }

  footer gzip_footer {
    field crc32: uint32;
    field input_size: uint32;
  }

  body gzip_body {
    if (header.flags & 0x04) {
      field extra_length: uint16;
      field extra: bytes[extra_length];
    }

    if (header.flags & 0x08) {
      field filename: string;
      field zero_terminator: uint8 = 0;
    }

    if (header.flags & 0x10) {
      field comment: string;
      field zero_terminator: uint8 = 0;
    }

    if (header.flags & 0x02) {
      field header_crc16: uint16;
    }

    field compressed_data: bytes;
  }

  grammar {
    start: gzip_header gzip_body gzip_footer;
  }
}