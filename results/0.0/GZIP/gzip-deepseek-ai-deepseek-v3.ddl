GZIP {
  header: GZIPHeader;
  extra_fields: Optional<ExtraFields>;
  filename: Optional<Filename>;
  comment: Optional<Comment>;
  header_crc16: Optional<HeaderCRC16>;
  compressed_data: CompressedData;
  trailer: GZIPTrailer;
}

GZIPHeader {
  id1: uint8 = 0x1F;
  id2: uint8 = 0x8B;
  compression_method: uint8 = 0x08;
  flags: Flags;
  modification_time: uint32;
  extra_flags: uint8;
  operating_system: uint8;
}

Flags {
  ftext: bit;
  fhcrc: bit;
  fextra: bit;
  fname: bit;
  fcomment: bit;
  reserved: bit[3];
}

ExtraFields {
  xlen: uint16;
  extra_data: uint8[xlen];
}

Filename {
  name: StringNullTerminated;
}

Comment {
  text: StringNullTerminated;
}

HeaderCRC16 {
  crc16: uint16;
}

CompressedData {
  data: uint8[];
}

GZIPTrailer {
  crc32: uint32;
  isize: uint32;
}

StringNullTerminated {
  chars: uint8[];
  null_terminator: uint8 = 0x00;
}

Optional<T> {
  present: bool;
  value: T if present;
}