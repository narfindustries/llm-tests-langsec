domain tiff {
  types {
    uint16  : 2;
    uint32  : 4;
    uint64  : 8;
    string  : sequence byte;
  }

  structs {
    IFD {
      entry_count: uint16;
      entries: array IFDEntry entry_count;
      next_ifd: uint32;
    }

    IFDEntry {
      tag: uint16;
      type: uint16;
      count: uint32;
      value: choice type {
        uint16: array uint16 count;
        uint32: array uint32 count;
        uint64: array uint64 count;
        string: string;
      }
    }
  }

  root IFD;
}