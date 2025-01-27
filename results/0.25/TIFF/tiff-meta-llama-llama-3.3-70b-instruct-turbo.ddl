domain tiff {
  types {
    uint8  : 1 byte;
    uint16 : 2 bytes;
    uint32 : 4 bytes;
  }

  type tiff_header {
    byte_order: uint16;
    version: uint16;
    iftl_offset: uint32;
  }

  type ifd {
    entry_count: uint16;
    entries: array[entry_count] of ifd_entry;
    next_ifd_offset: uint32;
  }

  type ifd_entry {
    tag: uint16;
    type: uint16;
    count: uint32;
    value: bytes;
  }

  grammar {
    tiff_file: tiff_header ifd;
  }
}