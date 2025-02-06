seq gzip_file {
  id1: byte = 0x1f
  id2: byte = 0x8b
  cm: byte = 8
  flags: byte
  mtime: uint32le
  xfl: byte
  os: byte
}

seq gzip_extra {
  xlen: uint16le
  extra: bytes[xlen]
}

seq gzip_header {
  gzip_file
  extra: if ((flags & 0x04) != 0) {
    gzip_extra
  }
  fname: if ((flags & 0x08) != 0) {
    string null_terminated
  }
  fcomment: if ((flags & 0x10) != 0) {
    string null_terminated
  }
  hdr_crc: if ((flags & 0x02) != 0) {
    uint16le
  }
}

seq gzip_compressed {
  compr_len: uint32le
  compr_data: bytes[compr_len]
  isize: uint32le
}

seq gzip {
  gzip_header
  gzip_compressed
}