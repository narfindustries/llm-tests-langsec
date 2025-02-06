TIFFImage {
  Header {
    ByteOrder: uint16 where match {
      0x4949 => "LittleEndian",
      0x4D4D => "BigEndian"
    };
    Magic: uint16 where == 42;
    IFDOffset: uint32;
  };
  IFDEntry {
    Tag: uint16;
    DataType: uint16 where match {
      1 => "BYTE",
      2 => "ASCII",
      3 => "SHORT",
      4 => "LONG",
      5 => "RATIONAL",
      6 => "SBYTE",
      7 => "UNDEFINED",
      8 => "SSHORT",
      9 => "SLONG",
      10 => "SRATIONAL",
      11 => "FLOAT",
      12 => "DOUBLE"
    };
    Count: uint32;
    ValueOrOffset: switch(DataType) {
      "BYTE" if Count <= 4 => uint8[Count];
      "ASCII" if Count <= 4 => string(Count);
      "SHORT" if Count <= 2 => uint16[Count];
      "LONG" if Count <= 1 => uint32[Count];
      "RATIONAL" if Count <= 1 => [uint32; uint32];
      "SBYTE" if Count <= 4 => int8[Count];
      "UNDEFINED" if Count <= 4 => uint8[Count];
      "SSHORT" if Count <= 2 => int16[Count];
      "SLONG" if Count <= 1 => int32[Count];
      "SRATIONAL" if Count <= 1 => [int32; int32];
      "FLOAT" if Count <= 1 => float32[Count];
      "DOUBLE" if Count <= 1 => float64[Count];
      else => uint32;
    };
  };
  IFD {
    EntryCount: uint16;
    Entries: IFDEntry[EntryCount];
    NextIFDOffset: uint32;
  };
  TIFFStructure {
    Header: Header;
    IFDs: {
      CurrOffset: Header.IFDOffset;
      while (CurrOffset != 0) {
        IFD: IFD where .Entries, .NextIFDOffset;
        CurrOffset: IFD.NextIFDOffset;
      }
    };
  };
};