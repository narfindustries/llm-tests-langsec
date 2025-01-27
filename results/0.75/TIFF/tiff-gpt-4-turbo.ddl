module TIFF;

import std::bitstream;
import std::bytes::be_u16;
import std::bytes::be_u32;

type IfdEntry = struct {
  tag : be_u16;
  datatype : be_u16;
  count : be_u32;
  value_offset : be_u32;
};

type Ifd = struct {
  num_entries : be_u16;
  entries : IfdEntry[num_entries];
  next_ifd : be_u32;
};

type TiffHeader = struct {
  endian_tag : bitstream[2*8];
  magic_number : be_u16;
  ifd0_offset : be_u32;
};

type TiffFile = struct {
  header : TiffHeader;
  ifd0 : Ifd if header.ifd0_offset > 0 -> seek(header.ifd0_offset);
};

entrypoint : TiffFile;