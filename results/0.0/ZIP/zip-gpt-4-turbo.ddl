type U2 = uint16 : little;
type U4 = uint32 : little;

struct ZipLocalFileHeader {
  U4 signature = 0x04034b50;
  U2 versionNeededToExtract;
  U2 generalPurposeBitFlag;
  U2 compressionMethod;
  U2 lastModFileTime;
  U2 lastModFileDate;
  U4 crc32;
  U4 compressedSize;
  U4 uncompressedSize;
  U2 fileNameLength;
  U2 extraFieldLength;
  byte[fileNameLength] fileName;
  byte[extraFieldLength] extraField;
}

struct ZipCentralDirectoryFileHeader {
  U4 signature = 0x02014b50;
  U2 versionMadeBy;
  U2 versionNeededToExtract;
  U2 generalPurposeBitFlag;
  U2 compressionMethod;
  U2 lastModFileTime;
  U2 lastModFileDate;
  U4 crc32;
  U4 compressedSize;
  U4 uncompressedSize;
  U2 fileNameLength;
  U2 extraFieldLength;
  U2 fileCommentLength;
  U2 diskNumberStart;
  U2 internalFileAttributes;
  U4 externalFileAttributes;
  U4 relativeOffsetOfLocalHeader;
  byte[fileNameLength] fileName;
  byte[extraFieldLength] extraField;
  byte[fileCommentLength] fileComment;
}

struct ZipEndOfCentralDirectoryRecord {
  U4 signature = 0x06054b50;
  U2 numberOfThisDisk;
  U2 diskWhereCentralDirectoryStarts;
  U2 numberOfCentralDirectoryRecordsOnThisDisk;
  U2 totalNumberOfCentralDirectoryRecords;
  U4 sizeOfCentralDirectory;
  U4 offsetOfStartOfCentralDirectory;
  U2 zipFileCommentLength;
  byte[zipFileCommentLength] zipFileComment;
}

struct ZipFile {
  list[ZipLocalFileHeader] localFiles;
  list[ZipCentralDirectoryFileHeader] centralDirectory;
  ZipEndOfCentralDirectoryRecord endOfCentralDirectoryRecord;
}