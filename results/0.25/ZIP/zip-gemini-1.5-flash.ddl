data ZipFile = ZipFile {
  centralDirectory : [CentralDirectoryEntry],
  endOfCentralDirectory : EndOfCentralDirectoryRecord
}

data CentralDirectoryEntry = CentralDirectoryEntry {
  signature : Constant { value = 0x02014b50, size = 4 },
  versionMadeBy : UInt16,
  versionNeededToExtract : UInt16,
  bitFlag : UInt16,
  method : UInt16,
  lastModTime : UInt16,
  lastModDate : UInt16,
  crc32 : UInt32,
  compressedSize : UInt32,
  uncompressedSize : UInt32,
  fileNameLength : UInt16,
  extraFieldLength : UInt16,
  fileCommentLength : UInt16,
  diskNumberStart : UInt16,
  internalAttributes : UInt16,
  externalAttributes : UInt32,
  localHeaderOffset : UInt32,
  fileName : Bytes fileNameLength,
  extraField : Bytes extraFieldLength,
  fileComment : Bytes fileCommentLength
}

data EndOfCentralDirectoryRecord = EndOfCentralDirectoryRecord {
  signature : Constant { value = 0x06054b50, size = 4 },
  diskNumber : UInt16,
  diskWithCentralDirectory : UInt16,
  entriesThisDisk : UInt16,
  totalEntries : UInt16,
  size : UInt32,
  offset : UInt32,
  commentLength : UInt16,
  comment : Bytes commentLength
}

data LocalFileHeader = LocalFileHeader {
  signature : Constant { value = 0x04034b50, size = 4 },
  version : UInt16,
  bitFlag : UInt16,
  method : UInt16,
  lastModTime : UInt16,
  lastModDate : UInt16,
  crc32 : UInt32,
  compressedSize : UInt32,
  uncompressedSize : UInt32,
  fileNameLength : UInt16,
  extraFieldLength : UInt16,
  fileName : Bytes fileNameLength,
  extraField : Bytes extraFieldLength,
  data : Bytes compressedSize
}
