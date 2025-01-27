module TiffFile

import BasicTypes

-- TIFF Header Structure
struct TiffHeader {
  endianness          : EndianTag;
  magic_number        : u16;
  ifd_offset          : u32;
}

-- Possible TIFF field value types
enum u16 EndianTag {
  II = 0x4949  -- little-endian
  MM = 0x4D4D  -- big-endian
}

struct IfdEntry {
  tag                 : u16;
  type                : u16;
  count               : u32;
  value_offset        : u32;
}

struct Ifd {
  num_entries         : u16;
  entries             : IfdEntry[num_entries];
  next_ifd_offset     : u32;
}

-- Basic types
alias u8 = uint<8>
alias u16 = uint<16>
alias u32 = uint<32>

-- TIFF file format
struct TiffFile {
  header              : TiffHeader;
  ifds                : Ifd[];

  let offset = header.ifd_offset;
  let rec ReadIfds(offset_: u32) =
    if offset_ == 0 then
      []
    else 
      ifd = Ifd.from_position(offset_);
      ifd + ReadIfds(ifd.next_ifd_offset);
  }
  where 
    ifds = ReadIfds(header.ifd_offset);
}