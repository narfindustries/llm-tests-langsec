domain png_image {
  little_endian;

  type IHDR {
    uint32 width;
    uint32 height;
    uint8 bit_depth;
    uint8 color_type;
    uint8 compression_method;
    uint8 filter_method;
    uint8 interlace_method;
  }

  type IDAT {
    uint32 crc;
    bytes data;
  }

  type IEND {
    uint32 crc;
  }

  type Chunk {
    string type = string(4);
    bytes data;
    uint32 crc = crc32(type, data);
  }

  type PngImage {
    bytes signature = bytes([137, 80, 78, 71, 13, 10, 26, 10]);
    IHDR ihdr;
    array(Chunk) chunks;
    IEND iend;
  }

  root_type PngImage;
}