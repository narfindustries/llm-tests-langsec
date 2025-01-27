def Main = {
    def header = {
        ID1     = 0x1f;
        ID2     = 0x8b;
        CM      = 0x08;
        FLG     = $byte;
        MTIME   = $uint32;
        XFL     = $byte;
        OS      = $byte;
        @check (ID1 == 0x1f && ID2 == 0x8b)
    }

    def extra_field = {
        XLEN    = $uint16;
        XDATA   = $bytes XLEN
    }

    def name_field = {
        FNAME   = $until1 0x00
    }

    def comment_field = {
        FCOMM   = $until1 0x00
    }

    def optional_fields = {
        @if ((FLG & 0x04) != 0) extra_field;
        @if ((FLG & 0x08) != 0) name_field;
        @if ((FLG & 0x10) != 0) comment_field
    }

    def trailer = {
        CRC32   = $uint32;
        ISIZE   = $uint32
    }

    def compressed_blocks = {
        CDATA   = $bytes ($remaining - 8)
    }

    header;
    optional_fields;
    compressed_blocks;
    trailer
}