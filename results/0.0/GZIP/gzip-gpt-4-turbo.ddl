module GZIP {

  import DAEDALUS::BitStream;

  struct GzipFile {
    header: GzipHeader,
    data: bytes(header.len),
    footer: GzipFooter
  }

  struct GzipHeader {
    id1: u8 = 0x1f,
    id2: u8 = 0x8b,
    compressionMethod: u8,
    flags: u8,
    mtime: u32,
    extraFlags: u8,
    os: u8,
    extra: optional<ExtraField> if flags & 0b100,
    filename: optional<cstring> if flags & 0b1000,
    comment: optional<cstring> if flags & 0b10000,
    headerCrc: optional<u16> if flags & 0b10
  }

  struct ExtraField {
    xlen: u16,
    data: bytes(xlen)
  }

  struct GzipFooter {
    crc32: u32,
    len: u32
  }

  bitstream GzipStream {
    file: GzipFile
  }
}