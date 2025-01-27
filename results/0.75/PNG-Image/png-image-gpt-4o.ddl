namespace PNGImage

def IHDR = struct {
    width: u4be,
    height: u4be,
    bitDepth: u1,
    colorType: u1,
    compressionMethod: u1,
    filterMethod: u1,
    interlaceMethod: u1
}

def Chunk = struct {
    length: u4be,
    type: string(4),
    data: bytes(length),
    crc: u4be
}

def PNGImage = struct {
    signature: magic_bytes("\x89PNG\r\n\x1a\n"),
    chunks: list_until(Chunk, flag_is_set(_, type == "IEND"))
}