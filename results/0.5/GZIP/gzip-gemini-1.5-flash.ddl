gzip = struct {
    id1: u8 := 0x1f;
    id2: u8 := 0x8b;
    cm: u8;
    flg: u8;
    mtime: u32;
    xfl: u8;
    os: u8;
    extra: if (flg & 0x04) { size: u16; data: bytes:size; };
    fname: if (flg & 0x08) { name: string; };
    comment: if (flg & 0x10) { comment: string; };
    hcrc: if (flg & 0x02) { hcrc: u16; };
    compressed: bytes;
    crc32: u32;
    isize: u32;
};

string = seq u8;
bytes = seq u8;

