datadef ZipFile {
  centralDirectory: CentralDirectory,
  endOfCentralDirectoryRecord: EndOfCentralDirectoryRecord,
  digitalSignature: DigitalSignature?,
}

datadef CentralDirectory {
  entries: seq(CentralDirectoryEntry),
}

datadef CentralDirectoryEntry {
  signature: uint32 := 0x02014b50,
  versionMadeBy: uint16,
  versionNeededToExtract: uint16,
  generalPurposeBitFlag: uint16,
  compressionMethod: uint16,
  lastModFileTime: uint16,
  lastModFileDate: uint16,
  crc32: uint32,
  compressedSize: uint32,
  uncompressedSize: uint32,
  fileNameLength: uint16,
  extraFieldLength: uint16,
  fileCommentLength: uint16,
  diskNumberStart: uint16,
  internalFileAttributes: uint16,
  externalFileAttributes: uint32,
  relativeOffsetOfLocalHeader: uint32,
  fileName: bytes(fileNameLength),
  extraField: bytes(extraFieldLength),
  fileComment: bytes(fileCommentLength),
}

datadef LocalFileHeader {
  signature: uint32 := 0x04034b50,
  versionNeededToExtract: uint16,
  generalPurposeBitFlag: uint16,
  compressionMethod: uint16,
  lastModFileTime: uint16,
  lastModFileDate: uint16,
  crc32: uint32,
  compressedSize: uint32,
  uncompressedSize: uint32,
  fileNameLength: uint16,
  extraFieldLength: uint16,
  fileName: bytes(fileNameLength),
  extraField: bytes(extraFieldLength),
  fileData: bytes,
}

datadef EndOfCentralDirectoryRecord {
  signature: uint32 := 0x06054b50,
  diskNumber: uint16,
  startDisk: uint16,
  numEntriesThisDisk: uint16,
  numEntriesTotal: uint16,
  sizeOfCentralDirectory: uint32,
  offsetOfCentralDirectory: uint32,
  commentLength: uint16,
  comment: bytes(commentLength),
}

datadef DigitalSignature {
  // Placeholder - Complex structure; Refer to PKWARE specification for details.
  data: bytes,
}
