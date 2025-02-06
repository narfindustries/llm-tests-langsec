def GZip {
    magic1 byte == 0x1f
    magic2 byte == 0x8b
    compression_method byte == 0x08
    flags bitfield[8] {
        text 1
        hcrc 1
        extra 1
        name 1
        comment 1
        reserved 3
    }
    mtime uint32
    xflags byte
    os byte

    if (flags.extra) {
        extra_length uint16
        extra_data byte[extra_length]
    }

    if (flags.name) {
        filename byte[] until 0x00
    }

    if (flags.comment) {
        comment byte[] until 0x00
    }

    if (flags.hcrc) {
        header_crc uint16
    }

    data byte[] until $
    crc32 uint32
    input_size uint32
}

def Main = GZip