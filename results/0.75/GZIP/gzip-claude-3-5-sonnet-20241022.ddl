def Main = {
    def header = { 
        magic1: 0x1f;
        magic2: 0x8b;
        cm: 8;
        flg: uint 8;
        mtime: uint 32 LE;
        xfl: uint 8;
        os: uint 8
    }

    def member = {
        hdr: header;
        compressed_blocks: $([0-9a-fA-F]+)*;
        crc32: uint 32 LE;
        isize: uint 32 LE
    }

    members: member[]
}