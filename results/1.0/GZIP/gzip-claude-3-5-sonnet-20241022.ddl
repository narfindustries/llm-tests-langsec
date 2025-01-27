def Main = gzipFile

def gzipFile = {
    header : gzipHeader
    blocks : compressed_blocks
    footer : gzipFooter
}

def gzipHeader = {
    id1     = 0x1f
    id2     = 0x8b
    cm      = 0x08
    flg     = !uint8
    mtime   = !uint32
    xfl     = !uint8
    os      = !uint8
}

def compressed_blocks = {
    block* until BlockEnd
}

def block = {
    bFinal = !uint1
    bType  = !uint2
    @check (bType == 0 || bType == 1 || bType == 2)
    
    blockData = case bType of {
        0 => uncompressedBlock
        1 => fixedHuffmanBlock
        2 => dynamicHuffmanBlock
    }
}

def BlockEnd = {
    $$ == 1
} where {
    $$ = currentBlock.bFinal
}

def uncompressedBlock = {
    @padToByte
    len     = !uint16
    nlen    = !uint16
    @check (len == (~nlen & 0xFFFF))
    data    = !byte[len]
}

def fixedHuffmanBlock = !bit*
def dynamicHuffmanBlock = !bit*

def gzipFooter = {
    crc32   = !uint32
    isize   = !uint32
}

def uint1  = !uint(1)
def uint2  = !uint(2)
def uint8  = !uint(8)
def uint16 = !uint(16)
def uint32 = !uint(32)

def padToByte = null where {
    $$ = SetNextByteAligned
}