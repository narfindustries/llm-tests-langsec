grammar png_image_meta {
  skip whitespace: /\s*/;

  rule png_header {
    "PNG" ~ "\r\n" ~ [0x1a];
    uint32be: png_signature;
  }

  rule ihdr_chunk {
    bytes: chunk_type = "IHDR";
    uint32be: chunk_length = 13;
    uint32be: width;
    uint32be: height;
    byte: bit_depth;
    byte: color_type;
    byte: compression_method;
    byte: filter_method;
    byte: interlace_method;
    bytes: crc;
  }

  rule idat_chunk {
    bytes: chunk_type = "IDAT";
    uint32be: chunk_length;
    bytes: zipped_image_data;
    bytes: crc;
  }

  rule iend_chunk {
    bytes: chunk_type = "IEND";
    uint32be: chunk_length = 0;
    bytes: crc;
  }

  rule png_image {
    png_header;
    ihdr_chunk;
    idat_chunk;
    iend_chunk;
  }
}