definition Gzip {
  // GZIP file format as per RFC 1952
  struct GzipFile {
    Header header;
    CompressedData compressed_data;
    uint32 crc32;
    uint32 isize;
  }

  struct Header {
    uint8 id1; // ID1 should be 0x1f
    uint8 id2; // ID2 should be 0x8b
    uint8 compression_method; // Compression method (8 = deflate)
    Flags flags;
    uint32 mtime; // Modification time
    uint8 xflags; // Extra flags
    uint8 os; // Operating system
    if (flags.fextra) {
      ExtraField extra;
    }
    if (flags.fname) {
      NullTerminatedString original_filename;
    }
    if (flags.fcomment) {
      NullTerminatedString comment;
    }
    if (flags.fhcrc) {
      uint16 header_crc16;
    }
  }

  struct Flags {
    uint8 reserved:3;
    uint8 fhcrc:1; // Header CRC16 present
    uint8 fextra:1; // Extra field present
    uint8 fname:1; // Original file name present
    uint8 fcomment:1; // Comment present
    uint8 ftext:1; // File is ASCII text
  }

  struct ExtraField {
    uint16 xlen; // Length of extra field
    bytes[xlen] extra_data;
  }

  struct CompressedData {
    bytes[] data; // Compressed data stream
  }

  struct NullTerminatedString {
    char[] str until(terminator: 0);
  }
}