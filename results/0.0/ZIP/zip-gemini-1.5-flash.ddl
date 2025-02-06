module Zip where

import Data.ByteString (ByteString)
import Data.Word (Word16, Word32)
import Daedalus.Type.Binary

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
  lastModTime :: Word16,
  lastModDate :: Word16,
  crc32 :: Word32,
  compressedSize :: Word32,
  uncompressedSize :: Word32,
  filenameLength :: Word16,
  extraFieldLength :: Word16,
  filename :: ByteString,
  extraField :: ByteString,
  compressedData :: ByteString
}

data CentralDirectory = CentralDirectory {
  centralDirectoryEntries :: [CentralDirectoryEntry],
  commentLength :: Word16,
  comment :: ByteString
}

data CentralDirectoryEntry = CentralDirectoryEntry {
  signature :: Word32,
  versionMadeBy :: Word16,
  versionNeededToExtract :: Word16,
  generalPurposeBitFlag :: Word16,
  compressionMethod :: Word16,
  lastModTime :: Word16,
  lastModDate :: Word16,
  crc32 :: Word32,
  compressedSize :: Word32,
  uncompressedSize :: Word32,
  filenameLength :: Word16,
  extraFieldLength :: Word16,
  fileCommentLength :: Word16,
  diskNumberStart :: Word16,
  internalFileAttributes :: Word16,
  externalFileAttributes :: Word32,
  localHeaderOffset :: Word32,
  filename :: ByteString,
  extraField :: ByteString,
  fileComment :: ByteString
}

data EndOfCentralDirectory = EndOfCentralDirectory {
  signature :: Word32,
  numberOfThisDisk :: Word16,
  numberOfDiskWithStartOfCentralDirectory :: Word16,
  totalNumberOfEntriesInCentralDirectoryOnThisDisk :: Word16,
  totalNumberOfEntriesInCentralDirectory :: Word16,
  sizeOfCentralDirectory :: Word32,
  offsetOfStartOfCentralDirectory :: Word32,
  commentLength :: Word16,
  comment :: ByteString
}

instance Binary ZipFile where
  put = undefined
  get = undefined

instance Binary LocalFileHeader where
  put = undefined
  get = undefined

instance Binary CentralDirectory where
  put = undefined
  get = undefined

instance Binary CentralDirectoryEntry where
  put = undefined
  get = undefined

instance Binary EndOfCentralDirectory where
  put = undefined
  get = undefined

instance Binary ByteString where
  put = undefined
  get = undefined

instance Binary Word32 where
  put = undefined
  get = undefined

instance Binary Word16 where
  put = undefined
  get = undefined

instance Binary a => Binary [a] where
  put xs = do
    put (length xs)
    mapM_ put xs
  get = do
    n <- get
    replicateM n get
