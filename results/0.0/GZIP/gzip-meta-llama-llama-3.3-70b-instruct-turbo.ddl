domain gzip {
  import byteorder;

  type gzip_file = struct {
    id1: u8,
    id2: u8,
    cm: u8,
    flags: u8,
    mtime: u32,
    xflags: u8,
    os: u8,
  };

  type compressed_member = struct {
    extra_flags: u8,
    os: u8,
    header_crc16: u16,
    compressed_data: bytes,
    crc32: u32,
    isize: u32,
  };

  grammar gzip_file {
    id1: '0x1f',
    id2: '0x8b',
    cm: '0x08',
    flags,
    mtime,
    xflags,
    os,
    compressed_member,
  };

  grammar compressed_member {
    extra_flags,
    os,
    header_crc16,
    compressed_data,
    crc32,
    isize,
  };

  grammar flags {
    fdict: u1,
    fextra: u1,
    fcomment: u1,
    freserved: u3,
    ftext: u1,
    fhcrc: u1,
  };

  grammar extra_flags {
    eextra: u1,
    ecomment: u1,
    ereserved: u5,
    etext: u1,
    ehcrc: u1,
  };

  grammar xflags {
    xlen: u8,
    xdata: bytes(xlen),
  };

  grammar os {
    os_type: u8,
  };

  grammar header_crc16 {
    crc: u16,
  };

  grammar compressed_data {
    data: bytes,
  };

  grammar crc32 {
    crc: u32,
  };

  grammar isize {
    size: u32,
  };
}