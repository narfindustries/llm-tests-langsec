domain tiff {
  type uint8  = uint(8);
  type uint16 = uint(16);
  type uint32 = uint(32);
  type uint64 = uint(64);
  type int8   = int(8);
  type int16  = int(16);
  type int32  = int(32);
  type int64  = int(64);

  type IFD = struct {
    entry_count: uint16,
    entries: array(entry_count) of IFDEntry,
    next_ifd: uint32,
  };

  type IFDEntry = struct {
    tag: uint16,
    type: uint16,
    count: uint32,
    value: bytes(4),
  };

  type TIFF = struct {
    byte_order: bytes(2),
    magic: uint16,
    ifd_offset: uint32,
    ifd: IFD,
  };

  grammar TIFFGrammar {
    TIFF: {
      byte_order: read(uint16),
      magic: read(uint16),
      ifd_offset: read(uint32),
      ifd: IFD,
    };

    IFD: {
      entry_count: read(uint16),
      entries: read(array(entry_count) of IFDEntry),
      next_ifd: read(uint32),
    };

    IFDEntry: {
      tag: read(uint16),
      type: read(uint16),
      count: read(uint32),
      value: read(bytes(4)),
    };
  };

  root_type TIFFGrammar.TIFF;
}