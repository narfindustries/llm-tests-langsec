// Daedalus specification for PNG file format
namespace png

// PNG file signature
def PNGSignature = struct {
    signature: bytes(8) == [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
}

// PNG chunk structure
def PNGChunk = struct {
    length: uint32be
    type: bytes(4)
    data: bytes(length)
    crc: uint32be
}

// PNG file structure
def PNGFile = struct {
    signature: PNGSignature
    chunks: list(PNGChunk) until (chunks.last.type == "IEND")
}

// Entry point for the PNG specification
def main = PNGFile
