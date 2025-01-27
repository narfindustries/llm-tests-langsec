module GZIP {

  import std::{u8, u16, u32, i32, array};

  struct GzipHeader {
    id1: u8 = 0x1F; // IDentification 1
    id2: u8 = 0x8B; // IDentification 2
    compressionMethod: u8;
    flags: Flags;
    mtime: u32; // Modification TIME
    extraFlags: u8;
    os: u8;

    extra: option[ExtraField] = if flags.FEXTRA { ExtraField } else { none };
    filename: option[array[u8]] = if flags.FNAME { until0 } else { none };
    comment: option[array[u8]] = if flags.FCOMMENT { until0 } else { none };
    headerCrc: option[u16] = if flags.FHCRC { u16 } else { none };
  }

  struct Flags {
    FTEXT: u8; // bit 0
    FHCRC: u8; // bit 1
    FEXTRA: u8; // bit 2
    FNAME: u8; // bit 3
    FCOMMENT: u8; // bit 4
    reserved: u8[3];
  }

  struct ExtraField {
    xLen: u16;
    xData: array[u8](xLen); // Extra FIELD data
  }

  struct Member {
    header: GzipHeader;
    body: array[u8];
    footer: Footer;
  }

  struct Footer {
    crc32: u32;
    inputSize: u32; // ISIZE, the size of the original (uncompressed) input data modulo 2^32
  }

  // Function to read until a null (0x00) byte is reached
  def until0 = array[u8] (n: u8) = (n != 0x00);

}