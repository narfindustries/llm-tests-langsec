namespace zip;

struct ZipFile {
    local uint32 signature = 0x04034b50;
    uint16 version;
    uint16 flags;
    uint16 compression;
    uint16 modTime;
    uint16 modDate;
    uint32 crc32;
    uint32 compressedSize;
    uint32 uncompressedSize;
    uint16 fileNameLength;
    uint16 extraFieldLength;
    string fileName[fileNameLength];
    uint8 extraField[extraFieldLength];
    uint8 compressedData[compressedSize];
}

struct CentralDirectoryFileHeader {
    local uint32 signature = 0x02014b50;
    uint16 versionMadeBy;
    uint16 versionNeeded;
    uint16 flags;
    uint16 compression;
    uint16 modTime;
    uint16 modDate;
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
    string fileName[fileNameLength];
    uint8 extraField[extraFieldLength];
    string fileComment[fileCommentLength];
}

struct EndOfCentralDirectoryRecord {
    local uint32 signature = 0x06054b50;
    uint16 diskNumber;
    uint16 centralDirectoryStartDisk;
    uint16 centralDirectoryRecordsOnDisk;
    uint16 totalCentralDirectoryRecords;
    uint32 centralDirectorySize;
    uint32 centralDirectoryOffset;
    uint16 commentLength;
    string comment[commentLength];
}

struct ZipArchive {
    ZipFile files[];
    CentralDirectoryFileHeader centralDirectory[];
    EndOfCentralDirectoryRecord endOfCentralDirectory;
}