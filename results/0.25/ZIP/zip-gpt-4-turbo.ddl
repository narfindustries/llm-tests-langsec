module ZIP;

struct LocalFileHeader {
  u32 signature = 0x04034b50;
  u16 versionNeeded;
  u16 generalPurposeBitFlag;
  u16 compressionMethod;
  u16 lastModFileTime;
  u16 lastModFileDate;
  u32 crc32;
  u32 compressedSize;
  u32 uncompressedSize;
  u16 fileNameLength;
  u16 extraFieldLength;
  array[u8] fileName[fileNameLength];
  array[u8] extraField[extraFieldLength];
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
  array[u8] fileName[fileNameLength];
  array[u8] extraField[extraFieldLength];
  array[u8] fileComment[fileCommentLength];
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
  array[u8] zipFileComment[zipFileCommentLength];
}

struct ZIPFile {
  array[LocalFileHeader] localFileHeaders;
  array[CentralDirectoryFileHeader] centralDirectoryFileHeaders;
  EndOfCentralDirectoryRecord endOfCentralDirectoryRecord;
}