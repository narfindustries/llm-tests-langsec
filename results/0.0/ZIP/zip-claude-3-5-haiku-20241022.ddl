schema ZipFile {
    localFileHeaders: List<LocalFileHeader>,
    centralDirectory: CentralDirectory,
    endOfCentralDirectory: EndOfCentralDirectory
}

struct LocalFileHeader {
    signature: UInt32 == 0x04034B50,
    versionNeededToExtract: UInt16,
    generalPurposeBitFlag: UInt16,
    compressionMethod: UInt16,
    lastModFileTime: UInt16,
    lastModFileDate: UInt16,
    crc32: UInt32,
    compressedSize: UInt32,
    uncompressedSize: UInt32,
    fileNameLength: UInt16,
    extraFieldLength: UInt16,
    fileName: String(fileNameLength),
    extraField: Bytes(extraFieldLength),
    fileData: Bytes(compressedSize)
}

struct CentralDirectory {
    fileHeaders: List<CentralDirectoryFileHeader>
}

struct CentralDirectoryFileHeader {
    signature: UInt32 == 0x02014B50,
    versionMadeBy: UInt16,
    versionNeededToExtract: UInt16,
    generalPurposeBitFlag: UInt16,
    compressionMethod: UInt16,
    lastModFileTime: UInt16,
    lastModFileDate: UInt16,
    crc32: UInt32,
    compressedSize: UInt32,
    uncompressedSize: UInt32,
    fileNameLength: UInt16,
    extraFieldLength: UInt16,
    fileCommentLength: UInt16,
    diskNumberStart: UInt16,
    internalFileAttributes: UInt16,
    externalFileAttributes: UInt32,
    relativeOffsetOfLocalHeader: UInt32,
    fileName: String(fileNameLength),
    extraField: Bytes(extraFieldLength),
    fileComment: String(fileCommentLength)
}

struct EndOfCentralDirectory {
    signature: UInt32 == 0x06054B50,
    numberOfThisDisk: UInt16,
    diskWithCentralDirectory: UInt16,
    totalEntriesOnThisDisk: UInt16,
    totalEntriesInCentralDirectory: UInt16,
    sizeOfCentralDirectory: UInt32,
    offsetOfCentralDirectory: UInt32,
    zipFileCommentLength: UInt16,
    zipFileComment: String(zipFileCommentLength)
}

struct Zip64EndOfCentralDirectory {
    signature: UInt32 == 0x06064B50,
    sizeOfZip64EndOfCentralDirectoryRecord: UInt64,
    versionMadeBy: UInt16,
    versionNeededToExtract: UInt16,
    numberOfThisDisk: UInt32,
    diskWithCentralDirectory: UInt32,
    totalEntriesOnThisDisk: UInt64,
    totalEntriesInCentralDirectory: UInt64,
    sizeOfCentralDirectory: UInt64,
    offsetOfCentralDirectory: UInt64
}