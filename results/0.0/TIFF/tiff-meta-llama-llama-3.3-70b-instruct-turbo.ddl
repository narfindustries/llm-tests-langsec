domain tiff {
  type uint8 = uint8;
  type uint16 = uint16;
  type uint32 = uint32;
  type uint64 = uint64;

  type ifd_entry_type = uint16;
  type ifd_entry_count = uint16;
  type ifd_entry_value = uint32;
  type ifd_offset = uint32;

  type tiff_header = struct {
    byte_order: uint16,
    version: uint16,
    ifd_offset: ifd_offset,
  };

  type ifd_entry = struct {
    tag: ifd_entry_type,
    type: ifd_entry_type,
    count: ifd_entry_count,
    value: ifd_entry_value,
  };

  type ifd = array<ifd_entry>;

  type tiff = struct {
    header: tiff_header,
    ifd: ifd,
  };

  syntax tiff_header = bytes(8) => {
    byte_order: uint16 @ 0,
    version: uint16 @ 2,
    ifd_offset: ifd_offset @ 4,
  };

  syntax ifd_entry = bytes(12) => {
    tag: ifd_entry_type @ 0,
    type: ifd_entry_type @ 2,
    count: ifd_entry_count @ 4,
    value: ifd_entry_value @ 8,
  };

  syntax ifd = array(ifd_entry);

  syntax tiff = bytes => {
    header: tiff_header @ 0,
    ifd: ifd @ header.ifd_offset,
  };
}