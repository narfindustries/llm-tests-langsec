format zip {
  struct LocalFileHeader {
    uint32 signature = 0x04034b50,
    uint16 versionNeededToExtract,
    uint16 generalPurposeBitFlag,
    uint16 compressionMethod,
    uint16 lastModifiedTime,
    uint16 lastModifiedDate,
    uint32 crc32,
    uint32 compressedSize,
    uint32 uncompressedSize,
    uint16 filenameLength,
    uint16 extraFieldLength,
    bytes filename,
    bytes extraField
  }

  struct CentralDirectory {
    uint32 signature = 0x02014b50,
    uint16 versionMadeBy,
    uint16 versionNeededToExtract,
    uint16 generalPurposeBitFlag,
    uint16 compressionMethod,
    uint16 lastModifiedTime,
    uint16 lastModifiedDate,
    uint32 crc32,
    uint32 compressedSize,
    uint32 uncompressedSize,
    uint16 filenameLength,
    uint16 extraFieldLength,
    uint16 fileCommentLength,
    uint16 diskNumberStart,
    uint16 internalFileAttributes,
    uint32 externalFileAttributes,
    uint32 localHeaderOffset,
    bytes filename,
    bytes extraField,
    bytes fileComment
  }

  struct EndOfCentralDirectory {
    uint32 signature = 0x06054b50,
    uint16 numberOfThisDisk,
    uint16 numberOfTheDiskWhereTheCentralDirectoryStarts,
    uint16 numberOfEntriesInTheCentralDirectoryOnThisDisk,
    uint16 numberOfEntriesInTheCentralDirectory,
    uint32 sizeOfTheCentralDirectory,
    uint32 offsetOfStartOfCentralDirectory,
    uint16 commentLength,
    bytes comment
  }

  struct DataDescriptor {
    uint32 crc32,
    uint32 compressedSize,
    uint32 uncompressedSize
  }

  struct ZipFile {
    array LocalFileHeader localFileHeaders,
    array CentralDirectory centralDirectory,
    EndOfCentralDirectory endOfCentralDirectory,
    array DataDescriptor dataDescriptors
  }
}