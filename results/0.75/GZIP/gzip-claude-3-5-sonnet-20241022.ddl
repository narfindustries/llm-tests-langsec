def GZIP = {
    magic: uint16;
    compression_method: uint8;
    flags: uint8;
    mtime: uint32;
    xfl: uint8;
    os: uint8;

    $if flags == 0x04$ {
        xlen: uint16;
        extra_field: bytes(xlen);
    }

    $if flags == 0x08$ {
        filename: /[^\x00]*\x00/;
    }

    $if flags == 0x10$ {
        comment: /[^\x00]*\x00/;
    }

    $if flags == 0x02$ {
        header_crc16: uint16;
    }

    compressed_data: bytes;
    crc32: uint32;
    isize: uint32;
}