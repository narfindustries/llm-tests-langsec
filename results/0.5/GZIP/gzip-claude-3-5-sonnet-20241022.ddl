def Main = {
    def header = {
        ID1 @< 0x1f;
        ID2 @< 0x8b;
        CM  @< 0x08;
        FLG @< uint8;
        MTIME @< uint32;
        XFL @< uint8;
        OS @< uint8
    }

    def fextra = {
        XLEN @< uint16;
        EXTRA_BYTES @< Array XLEN uint8
    }

    def fname = {
        FNAME_STR @< ZeroTerminatedString
    }

    def fcomment = {
        FCOMMENT_STR @< ZeroTerminatedString
    }

    def fhcrc = {
        CRC16 @< uint16
    }

    def optional_fields = {
        if (FLG & 0x04) != 0 { fextra };
        if (FLG & 0x08) != 0 { fname };
        if (FLG & 0x10) != 0 { fcomment };
        if (FLG & 0x02) != 0 { fhcrc }
    }

    def compressed_blocks = {
        COMPRESSED_DATA @< Remaining uint8
    }

    def trailer = {
        CRC32 @< uint32;
        ISIZE @< uint32
    }

    header;
    optional_fields;
    compressed_blocks;
    trailer
}