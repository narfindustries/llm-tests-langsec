endianness "big";

struct LocalFileHeader {
    u32 signature = 0x04034b50;
    u16 versionNeededToExtract;
    u16 generalPurposeBitFlag;
    u16 compressionMethod;
    u16 lastModFileTime;
    u16 lastModFileDate;
    u32 crc32;
    u32 compressedSize;
    u32 uncompressedSize;
    u16 fileNameLength;
    u16 extraFieldLength;
    u8[fileNameLength] fileName;
    u8[extraFieldLength] extraField;
    u8[compressedSize] compressedData;
    if (generalPurposeBitFlag & 0x0008) {
        DataDescriptor dataDescriptor;
    }
}

struct DataDescriptor {
    u32 crc32;
    u32 compressedSize;
    u32 uncompressedSize;
}

struct CentralDirectoryFileHeader {
    u32 signature = 0x02014b50;
    u16 versionMadeBy;
    u16 versionNeededToExtract;
    u16 generalPurposeBitFlag;
    u16 compressionMethod;
    u16 lastModFileTime;
    u16 lastModFileDate;
    u32 crc32;
    u32 compressedSize;
    u32 uncompressedSize;
    u16 fileNameLength;
    u16 extraFieldLength;
    u16 fileCommentLength;
    u16 diskNumberStart;
    u16 internalFileAttributes;
    u32 externalFileAttributes;
    u32 relativeOffsetOfLocalHeader;
    u8[fileNameLength] fileName;
    u8[extraFieldLength] extraField;
    u8[fileCommentLength] fileComment;
}

struct EndOfCentralDirectoryRecord {
    u32 signature = 0x06054b50;
    u16 numberOfThisDisk;
    u16 diskWhereCentralDirectoryStarts;
    u16 numberOfCentralDirectoryRecordsOnThisDisk;
    u16 totalNumberOfCentralDirectoryRecords;
    u32 sizeOfCentralDirectory;
    u32 offsetOfStartOfCentralDirectory;
    u16 zipFileCommentLength;
    u8[zipFileCommentLength] zipFileComment;
}

struct Zip64EndOfCentralDirectoryRecord {
    u32 signature = 0x06064b50;
    u64 sizeOfZip64EndOfCentralDirectoryRecord;
    u16 versionMadeBy;
    u16 versionNeededToExtract;
    u32 numberOfThisDisk;
    u32 diskWhereCentralDirectoryStarts;
    u64 numberOfCentralDirectoryRecordsOnThisDisk;
    u64 totalNumberOfCentralDirectoryRecords;
    u64 sizeOfCentralDirectory;
    u64 offsetOfStartOfCentralDirectory;
    u8[] extensibleDataSector;
}

struct Zip64EndOfCentralDirectoryLocator {
    u32 signature = 0x07064b50;
    u32 numberOfDiskWithStartOfZip64EndOfCentralDirectory;
    u64 relativeOffsetOfZip64EndOfCentralDirectoryRecord;
    u32 totalNumberOfDisks;
}

struct ZipFile {
    LocalFileHeader[] localFileHeaders;
    CentralDirectoryFileHeader[] centralDirectoryFileHeaders;
    EndOfCentralDirectoryRecord endOfCentralDirectoryRecord;
    if (endOfCentralDirectoryRecord.totalNumberOfCentralDirectoryRecords == 0xFFFF) {
        Zip64EndOfCentralDirectoryRecord zip64EndOfCentralDirectoryRecord;
        Zip64EndOfCentralDirectoryLocator zip64EndOfCentralDirectoryLocator;
    }
}