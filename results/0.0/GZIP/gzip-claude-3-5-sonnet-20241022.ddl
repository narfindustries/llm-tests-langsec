def Main = GZIP

def GZIP = {
    ID1     : uint 8 = 0x1f;
    ID2     : uint 8 = 0x8b;
    CM      : uint 8 = 0x08;
    FLG     : uint 8;
    MTIME   : uint 32;
    XFL     : uint 8;
    OS      : uint 8;
    
    if (FLG & 0x04) {  -- FEXTRA
        XLEN    : uint 16;
        EXTRA   : uint 8[XLEN];
    };
    
    if (FLG & 0x08) {  -- FNAME
        FNAME   : uint 8[] until 0x00;
    };
    
    if (FLG & 0x10) {  -- FCOMMENT
        FCOMMENT: uint 8[] until 0x00;
    };
    
    if (FLG & 0x02) {  -- FHCRC
        HCRC    : uint 16;
    };
    
    COMPRESSED_BLOCKS : uint 8[] until $$ == 0;
    
    CRC32   : uint 32;
    ISIZE   : uint 32;
}