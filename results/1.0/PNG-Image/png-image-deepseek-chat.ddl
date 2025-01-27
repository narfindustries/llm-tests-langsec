let PNG_SIGNATURE = 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A

type Chunk = struct {
    length: uint32,
    type: string(4),
    data: bytes(length),
    crc: uint32
}

type IHDR = struct {
    width: uint32,
    height: uint32,
    bit_depth: uint8,
    color_type: uint8,
    compression_method: uint8,
    filter_method: uint8,
    interlace_method: uint8
}

type PLTE = struct {
    entries: array(length / 3) of struct {
        red: uint8,
        green: uint8,
        blue: uint8
    }
}

type IDAT = struct {
    data: bytes(length)
}

type IEND = struct {}

type PNG = struct {
    signature: bytes(8) == PNG_SIGNATURE,
    chunks: repeat until eof of Chunk
}

let png = PNG