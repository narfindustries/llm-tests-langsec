GZIP : struct {
    header : struct {
        id1 : ubyte : assert(id1 == 0x1f); // ID1 should be 0x1f
        id2 : ubyte : assert(id2 == 0x8b); // ID2 should be 0x8b
        compressionMethod : ubyte : assert(compressionMethod == 0x08); // DEFLATE method
        flags : ubyte;
        modTime : uint32;
        extraFlags : ubyte;
        os : ubyte;
        extra : if (flags & 0x04 != 0) then struct {
            xlen : uint16;
            data : bytes(xlen);
        };
        name : if (flags & 0x08 != 0) then c_string;
        comment : if (flags & 0x10 != 0) then c_string;
        hcrc : if (flags & 0x02 != 0) then uint16;
    };
    compressed_data : bytes;
    crc32 : uint32;
    isize : uint32;
};