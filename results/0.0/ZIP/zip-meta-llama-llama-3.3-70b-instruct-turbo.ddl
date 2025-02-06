type LocalFileHeader {
  signature: uint16 = 0x04034b50,
  versionNeededToExtract: uint16,
  generalPurposeBitFlag: uint16,
  compressionMethod: uint16,
  lastModificationTime: uint16,
  lastModificationDate: uint16,
  crc32: uint32,
  compressedSize: uint32,
  uncompressedSize: uint32,
  filenameLength: uint16,
  extraFieldLength: uint16,
  filename: bytes[filenameLength],
  extraField: bytes[extraFieldLength]
}

type CentralDirectory {
  signature: uint32 = 0x02014b50,
  versionMadeBy: uint16,
  versionNeededToExtract: uint16,
  generalPurposeBitFlag: uint16,
  compressionMethod: uint16,
  lastModificationTime: uint16,
  lastModificationDate: uint16,
  crc32: uint32,
  compressedSize: uint32,
  uncompressedSize: uint32,
  filenameLength: uint16,
  extraFieldLength: uint16,
  fileCommentLength: uint16,
  diskNumberStart: uint16,
  internalFileAttributes: uint16,
  externalFileAttributes: uint32,
  relativeOffsetOfLocalHeader: uint32,
  filename: bytes[filenameLength],
  extraField: bytes[extraFieldLength],
  fileComment: bytes[fileCommentLength]
}

type EndOfCentralDirectory {
  signature: uint32 = 0x06054b50,
  numberOfThisDisk: uint16,
  numberOfTheDiskWhereTheCentralDirectoryStarts: uint16,
  numberOfEntriesInTheCentralDirectoryOnThisDisk: uint16,
  totalNumberOfEntriesInTheCentralDirectory: uint16,
  sizeOfTheCentralDirectory: uint32,
  offsetOfStartOfCentralDirectoryWithRespectToTheStartingDiskNumber: uint32
}

type ZipFile {
  localFileHeaders: array[LocalFileHeader],
  centralDirectories: array[CentralDirectory],
  endOfCentralDirectory: EndOfCentralDirectory
}

type DosDateTime {
  date: uint16,
  time: uint16
}

type InternalFileAttributes {
  fileType: uint8,
  fileFormat: uint8
}

type ExternalFileAttributes {
  filePermissions: uint16,
  fileType: uint8
}

type ExtraField {
  headerId: uint16,
  dataSize: uint16,
  data: bytes[dataSize]
}

type FileComment {
  comment: bytes
}

type ZipArchive {
  zipFile: ZipFile
}