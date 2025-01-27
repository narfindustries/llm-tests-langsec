namespace TIFF;

struct TiffHeader {
  endian: u16;
  magic: u16;
  ifd_offset: u32;
}

enum Endian : u16 {
  Little = 0x4949,
  Big = 0x4D4D
}

struct ImageFileDirectory {
  num_entries: u16;
  entries: Entry[num_entries];
  next_ifd_offset: u32;
}

struct Entry {
  tag: u16;
  type: u16;
  count: u32;
  value_offset: u32;
}

enum Tag : u16 {
  ImageWidth = 256,
  ImageLength = 257,
  BitsPerSample = 258,
  Compression = 259,
  PhotometricInterpretation = 262,
  StripOffsets = 273,
  RowsPerStrip = 278,
  StripByteCounts = 279,
  XResolution = 282,
  YResolution = 283,
  ResolutionUnit = 296
}

enum Type : u16 {
  BYTE = 1,
  ASCII = 2,
  SHORT = 3,
  LONG = 4,
  RATIONAL = 5
}

struct TiffFile {
  header: TiffHeader;
  ifds: ImageFileDirectory[];
}

@top TiffFile;