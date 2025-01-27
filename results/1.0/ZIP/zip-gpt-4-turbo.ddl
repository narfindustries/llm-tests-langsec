module ZIP where

import DaeDaLus.Lib
import DaeDaLus.Bits
import DaeDaLus.Parser

data EndOfCentralDir = EndOfCentralDir {
  signature       :: UInt32be -- expected 0x06054b50
, diskNum         :: UInt16be
, startDiskNum    :: UInt16be
, numEntriesDisk  :: UInt16be
, numEntriesTotal :: UInt16be
, sizeCentralDir  :: UInt32be
, offsetCentralDir:: UInt32be
, zipCommentLen   :: UInt16be
, zipComment      :: String zipCommentLen ISO8859_1
} deriving Show

data LocalFileHeader = LocalFileHeader {
  lSignature     :: UInt32be -- 0x04034b50
, versionNeeded  :: UInt16be
, flags          :: UInt16be
, compression    :: UInt16be
, modTime        :: UInt16be
, modDate        :: UInt16be
, crc32          :: UInt32be
, compSize       :: UInt32be
, uncompSize     :: UInt32be
, fileNameLen    :: UInt16be
, extraFieldLen  :: UInt16be
, fileName       :: String fileNameLen ISO8859_1
, extraField     :: Bytes extraFieldLen
} deriving Show

data CentralDir = CentralDir {
  cSignature     :: UInt32be -- expected 0x02014b50
, versionMadeBy  :: UInt16be
, versionNeededToExtract :: UInt16be
, flags          :: UInt16be
, compression    :: UInt16be
, modTime        :: UInt16be
, modDate        :: UInt16be
, crc32          :: UInt32be
, compSize       :: UInt32be
, uncompSize     :: UInt32be
, fileNameLen    :: UInt16be
, extraFieldLen  :: UInt16be
, fileCommentLen :: UInt16be
, diskNumStart   :: UInt16be
, intFileAttr    :: UInt16be
, extFileAttr    :: UInt32be
, relOffsetLH    :: UInt32be
, fileName       :: String fileNameLen ISO8859_1
, extraField     :: Bytes extraFieldLen
, fileComment    :: String fileCommentLen ISO8859_1
} deriving Show

localFileHeader = Find 0x04034b50 >> Struct LocalFileHeader
centralDirectory = Find 0x02014b50 >> Struct CentralDir
endOfCentralDir = Find 0x06054b50 >> Struct EndOfCentralDir

zipFile = Many localFileHeader .*. Many centralDirectory .*. endOfCentralDir