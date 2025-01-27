def Main = {
  Endian = BigEndian;
  Header;
  IFD*;
}

def Header = {
  byte_order : magic_number;
  version    : uint16;
  offset     : uint32;
  @assert byte_order == 0x4D4D || byte_order == 0x4949;
  @assert version == 42;
}

def magic_number = uint16

def IFD = {
  entry_count : uint16;
  entries     : Entry[entry_count];
  next_offset : uint32;
}

def Entry = {
  tag          : uint16;
  type         : uint16;
  count        : uint32;
  value_offset : uint32;
  @assert type >= 1 && type <= 12;
}

def TagType = Choose {
  BYTE      = 1;
  ASCII     = 2;
  SHORT     = 3;
  LONG      = 4;
  RATIONAL  = 5;
  SBYTE     = 6;
  UNDEFINED = 7;
  SSHORT    = 8;
  SLONG     = 9;
  SRATIONAL = 10;
  FLOAT     = 11;
  DOUBLE    = 12;
}

def TagId = Choose {
  ImageWidth               = 0x0100;
  ImageLength              = 0x0101;
  BitsPerSample           = 0x0102;
  Compression             = 0x0103;
  PhotometricInterpretation = 0x0106;
  StripOffsets            = 0x0111;
  SamplesPerPixel         = 0x0115;
  RowsPerStrip            = 0x0116;
  StripByteCounts        = 0x0117;
  XResolution             = 0x011A;
  YResolution             = 0x011B;
  ResolutionUnit          = 0x0128;
}