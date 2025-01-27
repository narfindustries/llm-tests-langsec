domain zip {
    byteorder little;
    complex ZIP {
        uint16 count: len(files) > 0 ? 2 : 0;
        bytes files: ZIPFile[count];
    }
    complex ZIPFile {
        uint32 signature: 0x04034b50;
        uint16 version: 20;
        uint16 flags: 0x0800;
        uint16 method: 0;
        uint16 lastmodtime: 0;
        uint16 lastmoddate: 0;
        uint32 crc32: crc32(uncompresseddata);
        uint32 compressedsize: len(compresseddata);
        uint32 uncompressedsize: len(uncompresseddata);
        bytes filename: "file.txt";
        bytes extrafield: "";
        bytes compresseddata: zlib(uncompresseddata);
        bytes uncompresseddata: "Hello, World!";
    }
}