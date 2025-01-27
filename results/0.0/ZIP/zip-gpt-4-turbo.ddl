module ZIP {

  import DAEDALUS::BitStream;

  struct LocalFileHeader {
    signature         : UInt32 = 0x04034b50;
    versionNeeded     : UInt16;
    generalPurpose    : UInt16;
    compressionMethod : UInt16;
    lastModTime       : UInt16;
    lastModDate       : UInt16;
    crc32             : UInt32;
    compressedSize    : UInt32;
    uncompressedSize  : UInt32;
    fileNameLength    : UInt16;
    extraFieldLength  : UInt16;
    fileName          : UInt8[fileNameLength];
    extraField        : UInt8[extraFieldLength];
  }

  struct CentralDirectoryFileHeader {
    signature         : UInt32 = 0x02014b50;
    versionMadeBy     : UInt16;
    versionNeeded     : UInt16;
    generalPurpose    : UInt16;
    compressionMethod : UInt16;
    lastModTime       : UInt16;
    lastModDate       : UInt16;
    crc32             : UInt32;
    compressedSize    : UInt32;
    uncompressedSize  : UInt32;
    fileNameLength    : UInt16;
    extraFieldLength  : UInt16;
    fileCommentLength : UInt16;
    diskNumberStart   : UInt16;
    internalFileAttr  : UInt16;
    externalFileAttr  : UInt32;
    localHeaderOffset : UInt32;
    fileName          : UInt8[fileNameLength];
    extraField        : UInt8[extraFieldLength];
    fileComment       : UInt8[fileCommentLength];
  }

  struct EndOfCentralDirectoryRecord {
    signature         : UInt32 = 0x06054b50;
    diskNumber        : UInt16;
    diskStart         : UInt16;
    numEntriesThisDisk: UInt16;
    numEntriesTotal   : UInt16;
    centralDirSize    : UInt32;
    centralDirOffset  : UInt32;
    commentLength     : UInt16;
    comment           : UInt8[commentLength];
  }

  struct ZIPFile {
    localFiles        : LocalFileHeader[*];
    centralDirectory  : CentralDirectoryFileHeader[*];
    endOfCentralDir   : EndOfCentralDirectoryRecord;
  }

  let zipFile = parse ZIPFile from BitStream;
}