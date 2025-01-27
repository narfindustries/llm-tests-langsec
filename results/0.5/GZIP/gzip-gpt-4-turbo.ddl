module GZIP {

  import DAEDALUS::Core;

  struct GzipFile {
    header: GzipHeader,
    data:   Bytes(header.len),
    footer: GzipFooter
  }

  struct GzipHeader {
    id1:     UInt8,              // ID1
    id2:     UInt8,              // ID2
    cm:      UInt8,              // Compression Method
    flg:     UInt8,              // Flags
    mtime:   UInt32,             // Modification Time
    xfl:     UInt8,              // Extra flags
    os:      UInt8,              // Operating system
    xlen:    UInt16 ? (flg & 0x04) != 0,
    xtra:    Bytes(xlen) ? (flg & 0x04) != 0,
    fname:   NullTermString ? (flg & 0x08) != 0,
    fcomment:NullTermString ? (flg & 0x10) != 0,
    hcrc:    UInt16 ? (flg & 0x02) != 0,
    len:     Compute <| UInt32 -> do
                guard (flg & 0x02) != 0 else 0;
                UInt32
              end
  }

  struct GzipFooter {
    crc32:   UInt32,
    isize:   UInt32
  }

}