domain GZIP {
  import binary;
  import std;

  payload gzip {
    uint16:lf magic = 0x1f8b;
    uint8:lf cm = 8;
    uint8:lf flags = 0;
    uint32:lf mtime = 0;
    uint8:lf xfl = 0;
    uint8:lf os = 0;

    if (flags & 0x01) {
      string:lf extra_len = divisible_by(2);
      bytes:lf extra = read(extra_len * 8);
    }

    if (flags & 0x02) {
      string:lf fname_len = until(0);
      bytes:lf fname = read(fname_len);
    }

    if (flags & 0x04) {
      string:lf comment_len = until(0);
      bytes:lf comment = read(comment_len);
    }

    if (flags & 0x08) {
      bytes:lf crc16 = read(16);
    }

    bytes:lf compressed_data = until([0x00, 0x00, 0xff, 0xff]);
    uint32:lf crc32 = read(32);
    uint32:lf isize = read(32);
  }
}