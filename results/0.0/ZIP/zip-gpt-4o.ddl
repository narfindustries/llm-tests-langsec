enum CompressionMethod : uint16 {
    STORE = 0,
    SHRINK = 1,
    IMPLODE = 6,
    DEFLATE = 8,
    DEFLATE64 = 9,
    BZIP2 = 12,
    LZMA = 14,
    PPMD = 98
}

struct LocalFileHeader {
    uint32 signature = 0x04034b50;
    uint16 versionNeededToExtract;
    uint16 generalPurposeBitFlag;
    CompressionMethod compressionMethod;
    uint16 lastModFileTime;
    uint16 lastModFileDate;
    uint32 crc32;
    uint32 compressedSize;
    uint32 uncompressedSize;
    uint16 fileNameLength;
    uint16 extraFieldLength;
    uint8[fileNameLength] fileName;
    uint8[extraFieldLength] extraField;
}

struct DataDescriptor {
    uint32 crc32;
    uint32 compressedSize;
    uint32 uncompressedSize;
}

struct CentralDirectoryFileHeader {
    uint32 signature = 0x02014b50;
    uint16 versionMadeBy;
    uint16 versionNeededToExtract;
    uint16 generalPurposeBitFlag;
    CompressionMethod compressionMethod;
    uint16 lastModFileTime;
    uint16 lastModFileDate;
    uint32 crc32;
    uint32 compressedSize;
    uint32 uncompressedSize;
    uint16 fileNameLength;
    uint16 extraFieldLength;
    uint16 fileCommentLength;
    uint16 diskNumberStart;
    uint16 internalFileAttributes;
    uint32 externalFileAttributes;
    uint32 relativeOffsetOfLocalHeader;
    uint8[fileNameLength] fileName;
    uint8[extraFieldLength] extraField;
    uint8[fileCommentLength] fileComment;
}

struct EndOfCentralDirectoryRecord {
    uint32 signature = 0x06054b50;
    uint16 numberOfThisDisk;
    uint16 diskWhereCentralDirectoryStarts;
    uint16 numberOfCentralDirectoryRecordsOnThisDisk;
    uint16 totalNumberOfCentralDirectoryRecords;
    uint32 sizeOfCentralDirectory;
    uint32 offsetOfStartOfCentralDirectory;
    uint16 zipFileCommentLength;
    uint8[zipFileCommentLength] zipFileComment;
}

struct ZIP64EndOfCentralDirectoryRecord {
    uint32 signature = 0x06064b50;
    uint64 sizeOfZIP64EndOfCentralDirectoryRecord;
    uint16 versionMadeBy;
    uint16 versionNeededToExtract;
    uint32 numberOfThisDisk;
    uint32 diskWhereCentralDirectoryStarts;
    uint64 numberOfCentralDirectoryRecordsOnThisDisk;
    uint64 totalNumberOfCentralDirectoryRecords;
    uint64 sizeOfCentralDirectory;
    uint64 offsetOfStartOfCentralDirectory;
    uint8[] zip64ExtensibleDataSector;
}

struct ZIP64EndOfCentralDirectoryLocator {
    uint32 signature = 0x07064b50;
    uint32 numberOfDiskWithStartOfZIP64EndOfCentralDirectory;
    uint64 relativeOffsetOfZIP64EndOfCentralDirectoryRecord;
    uint32 totalNumberOfDisks;
}

struct ZIPFile {
    LocalFileHeader[] localFileHeaders;
    DataDescriptor? dataDescriptor;
    CentralDirectoryFileHeader[] centralDirectoryFileHeaders;
    EndOfCentralDirectoryRecord endOfCentralDirectoryRecord;
    ZIP64EndOfCentralDirectoryRecord? zip64EndOfCentralDirectoryRecord;
    ZIP64EndOfCentralDirectoryLocator? zip64EndOfCentralDirectoryLocator;
}