domain tiff {
  type u1 = uint8;
  type u2 = uint16;
  type u4 = uint32;
  type i2 = int16;
  type i4 = int32;

  type tiff_hdr = struct {
    byte_order: u2,
    version: u2,
    ifd_offset: u4,
  };

  type ifd = struct {
    num_entries: u2,
    entries: array[num_entries] of ifd_entry,
    next_ifd_offset: u4,
  };

  type ifd_entry = struct {
    tag: u2,
    type: u2,
    count: u4,
    value: switch(type) {
      case 1: u1,
      case 2: string[count],
      case 3: u2,
      case 4: u4,
      case 5: struct { numerator: u4, denominator: u4 },
      case 7: bytes[count],
      case 9: i4,
      case 10: struct { numerator: i4, denominator: i4 },
    },
  };

  grammar tiff_file = tiff_hdr ifd*;
}