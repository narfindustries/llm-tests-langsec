data ZipFile = ZipFile {
  localFileHeaders :: [LocalFileHeader],
  centralDirectory :: CentralDirectory,
  endOfCentralDirectory :: EndOfCentralDirectory
}

data LocalFileHeader = LocalFileHeader {
  signature :: Word32,
  versionNeededToExtract :: Word16,
  generalPurposeBitFlag :: Word16,
  compressionMethod :: Word16,
  lastModifiedTime :: Word16,
  lastModifiedDate :: Word16,
  crc32 :: Word32,
  compressedSize :: Word32,
  uncompressedSize :: Word32,
  fileNameLength :: Word16,
  extraFieldLength :: Word16,
  fileName :: ByteString,
  extraField :: ByteString,
  fileData :: ByteString
}

data CentralDirectory = CentralDirectory {
  entries :: [CentralDirectoryEntry]
}

data CentralDirectoryEntry = CentralDirectoryEntry {
  signature :: Word32,
  versionMadeBy :: Word16,
  versionNeededToExtract :: Word16,
  generalPurposeBitFlag :: Word16,
  compressionMethod :: Word16,
  lastModifiedTime :: Word16,
  lastModifiedDate :: Word16,
  crc32 :: Word32,
  compressedSize :: Word32,
  uncompressedSize :: Word32,
  fileNameLength :: Word16,
  extraFieldLength :: Word16,
  fileCommentLength :: Word16,
  diskNumberStart :: Word16,
  internalFileAttributes :: Word16,
  externalFileAttributes :: Word32,
  relativeOffsetOfLocalHeader :: Word32,
  fileName :: ByteString,
  extraField :: ByteString,
  fileComment :: ByteString
}

data EndOfCentralDirectory = EndOfCentralDirectory {
  signature :: Word32,
  numberOfThisDisk :: Word16,
  numberOfTheDiskWithTheStartOfTheCentralDirectory :: Word16,
  totalNumberOfEntriesInTheCentralDirectoryOnThisDisk :: Word16,
  totalNumberOfEntriesInTheCentralDirectory :: Word16,
  sizeOfTheCentralDirectory :: Word32,
  offsetOfStartOfCentralDirectoryWithRespectToTheStartingDiskNumber :: Word32,
  zipFileCommentLength :: Word16,
  zipFileComment :: ByteString
}

type Word32 = Integer
type Word16 = Integer
type ByteString = [Word8]
type Word8 = Integer
