def PNG_SIGNATURE = 0x89504E470D0A1A0A

def IHDR = 0x49484452
def IDAT = 0x49444154
def IEND = 0x49454E44

def CRC32 = UInt32

def ChunkHeader = {
    length : UInt32
    type   : UInt32
}

def IHDRData = {
    width              : UInt32
    height             : UInt32
    bit_depth          : UInt8
    color_type         : UInt8
    compression_method : UInt8
    filter_method      : UInt8
    interlace_method   : UInt8
}

def Chunk = {
    header : ChunkHeader
    data   : Match header.type {
        IHDR => IHDRData
        _    => Array header.length UInt8
    }
    crc    : CRC32
}

def PNGImage = {
    signature : PNG_SIGNATURE
    ihdr      : Chunk
    idats     : Many Chunk
    iend      : Chunk
}

def Main = PNGImage