module Gzip {

  struct GzipFile {
    GzipMember[] members;
  }

  struct GzipMember {
    u8 id1 = 0x1f;
    u8 id2 = 0x8b;
    u8 cm = 0x08;
    Flg flg;
    u32 mtime;
    Xfl xfl;
    Os os;
    u16 xlen if flg.FEXTRA;
    u8[xlen] extraFields if flg.FEXTRA;
    CString filename if flg.FNAME;
    CString comment if flg.FCOMMENT;
    u16 headerCrc16 if flg.FHCRC;
    CompressedData compressedData;
    u32 crc32;
    u32 isize;
  }

  struct Flg {
    bool FTEXT : 1;
    bool FHCRC : 1;
    bool FEXTRA : 1;
    bool FNAME : 1;
    bool FCOMMENT : 1;
    u8 reserved : 3;
  }

  enum Xfl : u8 {
    Default = 0,
    MaximumCompression = 2,
    FastestCompression = 4
  }

  enum Os : u8 {
    FAT = 0,    // MS-DOS, OS/2, NT/Win32
    Amiga = 1,
    VMS = 2,    // or OpenVMS
    Unix = 3,
    VMCMS = 4,
    AtariTOS = 5,
    HPFS = 6,   // OS/2, NT
    Macintosh = 7,
    ZSystem = 8,
    CPM = 9,
    TOPS20 = 10,
    NTFS = 11,  // NT
    QDOS = 12,
    AcornRISCOS = 13,
    Unknown = 255
  }

  struct CString {
    stringz str;
  }

  struct CompressedData {
    bytes data; // Placeholder for compressed data block
  }

}