module GZIP {

  import DAEDALUS::BitManip;
  import DAEDALUS::Core;

  struct GzipFile {
    magic       : U16be = 0x1f8b; // GZIP magic header
    compression : U8;             // Compression method (8 = deflate)
    flags       : U8;             // Flags
    mtime       : U32le;          // Modification time
    xfl         : U8;             // Extra flags
    os          : U8;             // Operating system

    extra       : Maybe<ExtraField> = if flags & 0x04 != 0 then Just extraField else Nothing;
    fname       : Maybe<DAEDALUS::String> = if flags & 0x08 != 0 then Just zeroTerminatedString else Nothing;
    fcomment    : Maybe<DAEDALUS::String> = if flags & 0x10 != 0 then Just zeroTerminatedString else Nothing;
    crc16       : Maybe<U16le> = if flags & 0x02 != 0 then Just U16le else Nothing;

    compressedData : DAEDALUS::Bytes;
    crc32          : U32le; // CRC-32 checksum
    isize          : U32le; // Input size modulo 2^32
  }

  struct ExtraField {
    xlen   : U16le;
    xfield : DAEDALUS::Array<U8>(xlen);
  }

  let zeroTerminatedString : DAEDALUS::String = {
    let str = DAEDALUS::takeWhile(U8, \x -> x != 0);
    let _   = U8; // consume the zero byte
    str
  };

}