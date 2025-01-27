format PNG-Image:
    signature = 0x89 0x50 0x4E 0x47 0x0D 0x0A 0x1A 0x0A

    chunk IHDR:
        length: uint32
        type: bytes(4) = "IHDR"
        width: uint32
        height: uint32
        bit_depth: uint8
        color_type: uint8
        compression: uint8
        filter: uint8
        interlace: uint8
        crc: uint32

    chunk IDAT:
        length: uint32
        type: bytes(4) = "IDAT"
        data: bytes(length)
        crc: uint32

    chunk IEND:
        length: uint32
        type: bytes(4) = "IEND"
        crc: uint32

    file:
        signature
        header: IHDR
        image_data: IDAT
        trailer: IEND