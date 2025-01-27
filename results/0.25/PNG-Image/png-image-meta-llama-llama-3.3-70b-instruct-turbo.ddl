domain PNG-Image {
  types {
    byte        : uint8;
    uint16_be   : uint16  big-endian;
    uint32_be   : uint32  big-endian;
    uint16_le   : uint16  little-endian;
    uint32_le   : uint32  little-endian;
    string      : byte[ ];
    fourcc      : byte[4];
  }

  structs {
    IHDR {
      width         : uint32_be;
      height        : uint32_be;
      bit_depth     : byte;
      color_type    : byte;
      compression   : byte;
      filter        : byte;
      interlace     : byte;
    }

    chunk {
      length        : uint32_be;
      chunk_type    : fourcc;
      data          : byte[length];
      crc           : uint32_be;
    }

    PNG {
      signature     : byte[8] = [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a];
      ihdr          : IHDR;
      chunks        : chunk[ ];
      iend          : fourcc = [0x49, 0x45, 0x4e, 0x44];
    }
  }

  roots {
    png : PNG;
  }
}