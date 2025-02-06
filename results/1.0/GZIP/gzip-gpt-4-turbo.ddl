type U8 = UInt(8);
type U16 = UInt(16);
type U32 = UInt(32);

type Bool1 = UInt(1);

struct GZipFile {
  header: GZipHeader,
  compressed: VarUInt8[],
  footer: GZipFooter
}

struct GZipHeader {
  id1: U8[0x1f],
  id2: U8[0x8b],
  cm: U8[8],
  flags: GZipFlags,
  mtime: U32,
  xfl: U8,
  os: OS,
  optionalFields: OptionalFields
}

struct GZipFlags {
  FTEXT: Bool1,
  FHCRC: Bool1,
  FEXTRA: Bool1,
  FNAME: Bool1,
  FCOMMENT: Bool1,
  reserved: U8[0] : calc { 0 }
}

enum OS : U8 {
  FAT = 0,
  AMIGA = 1,
  VMS = 2,
  UNIX = 3,
  VM_CMS = 4,
  ATARI_TOS = 5,
  HPFS = 6,
  MACINTOSH = 7,
  Z_SYSTEM = 8,
  CP_M = 9,
  TOPS_20 = 10,
  NTFS = 11,
  QDOS = 12,
  ACORN_RISCOS = 13,
  UNKNOWN = 255
}

struct OptionalFields {
  extraField: U16 if GZipFlags.FEXTRA,
  fileName: CString if GZipFlags.FNAME,
  fileComment: CString if GZipFlags.FCOMMENT,
  headerCRC16: U16 if GZipFlags.FHCRC
}

struct GZipFooter {
  crc32: U32;
  isize: U32;
}

type CString = VarUInt8[] : until (uint($current) == 0);