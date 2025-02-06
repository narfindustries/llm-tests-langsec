struct ZipFile {
    localFileHeaders: list<LocalFileHeader>;
    centralDirectory: CentralDirectory;
    endOfCentralDirectory: EndOfCentralDirectory;
}

struct LocalFileHeader {
    signature: u32 = 0x04034B50;
    versionNeededToExtract: u16;
    generalPurposeBitFlag: u16;
    compressionMethod: u16;
    lastModFileTime: u16;
    lastModFileDate: u16;
    crc32: u32;
    compressedSize: u32;
    uncompressedSize: u32;
    fileNameLength: u16;
    extraFieldLength: u16;
    fileName: string(fileNameLength);
    extraField: bytes(extraFieldLength);
    fileData: bytes(compressedSize);
}

struct CentralDirectory {
    fileHeaders: list<CentralDirectoryFileHeader>;
}

struct CentralDirectoryFileHeader {
    signature: u32 = 0x02014B50;
    versionMadeBy: u16;
    versionNeededToExtract: u16;
    generalPurposeBitFlag: u16;
    compressionMethod: u16;
    lastModFileTime: u16;
    lastModFileDate: u16;
    crc32: u32;
    compressedSize: u32;
    uncompressedSize: u32;
    fileNameLength: u16;
    extraFieldLength: u16;
    fileCommentLength: u16;
    diskNumberStart: u16;
    internalFileAttributes: u16;
    externalFileAttributes: u32;
    localHeaderOffset: u32;
    fileName: string(fileNameLength);
    extraField: bytes(extraFieldLength);
    fileComment: string(fileCommentLength);
}

struct EndOfCentralDirectory {
    signature: u32 = 0x06054B50;
    numberOfThisDisk: u16;
    diskWhereStartOfCentralDirectory: u16;
    totalCentralDirectoryEntriesOnDisk: u16;
    totalCentralDirectoryEntries: u16;
    sizeOfCentralDirectory: u32;
    centralDirectoryOffset: u32;
    zipFileCommentLength: u16;
    zipFileComment: string(zipFileCommentLength);
}