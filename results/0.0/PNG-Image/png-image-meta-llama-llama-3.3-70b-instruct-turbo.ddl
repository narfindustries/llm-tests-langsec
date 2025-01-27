domain PNG-Image {
  types {
    uint8  : byte;
    uint16 : uint16;
    uint32 : uint32;
    uint64 : uint64;
    string : string;
  }

  type PNG_Image {
    field magic_number : byte[8];
    field ihdr : IHDR;
    field idat : IDAT;
    field iend : IEND;
  }

  type IHDR {
    field width  : uint32;
    field height : uint32;
    field bit_depth : byte;
    field color_type : byte;
    field compression_method : byte;
    field filter_method : byte;
    field interlace_method : byte;
  }

  type IDAT {
    field data : byte[*];
  }

  type IEND {
    field crc : uint32;
  }

  constraints {
    magic_number == [137, 80, 78, 71, 13, 10, 26, 10];
    ihdr.width > 0;
    ihdr.height > 0;
    ihdr.bit_depth in [1, 2, 4, 8, 16];
    ihdr.color_type in [0, 2, 3, 4, 6];
    ihdr.compression_method == 0;
    ihdr.filter_method == 0;
    ihdr.interlace_method in [0, 1];
  }

  root_type PNG_Image;
}