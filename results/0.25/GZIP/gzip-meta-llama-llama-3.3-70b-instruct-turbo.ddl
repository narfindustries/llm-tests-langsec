format binary {
  gzip_file {
    id1: byte = 0x1F
    id2: byte = 0x8B
    cm: byte = 8
    flags: byte
    mtime: uint32
    xfl: byte
    os: byte
    xlen: uint16 = if (flags & 0x04 != 0) then uint16 else 0
    extra: bytes = if (flags & 0x04 != 0) then bytes(xlen) else bytes(0)
    fname: string = if (flags & 0x08 != 0) then string(null_terminated) else ""
    fcomment: string = if (flags & 0x10 != 0) then string(null_terminated) else ""
    hcrc: uint16 = if (flags & 0x02 != 0) then uint16 else 0
    compr_data: bytes
    compr_len: uint32
    isize: uint32
  }
}