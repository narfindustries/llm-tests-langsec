module ZIP {

  import DAEDALUS::BitStream;

  struct LocalFileHeader {
    signature          : U32LE = 0x04034b50;
    versionNeeded      : U16LE;
    generalFlag        : U16LE;
    compressionMethod  : U16LE;
    lastModTime        : U16LE;
    lastModDate        : U16LE;
    crc32              : U32LE;
    compressedSize     : U32LE;
    uncompressedSize   : U32LE;
    fileNameLength     : U16LE;
    extraFieldLength   : U16LE;
    fileName           : U8[fileNameLength];
    extraField         : U8[extraFieldLength];
  }

  struct CentralDirectoryFileHeader {
    signature          : U32LE = 0x02014b50;
    versionMadeBy      : U16LE;
    versionNeeded      : U16LE;
    generalFlag        : U16LE;
    compressionMethod  : U16LE;
    lastModTime        : U16LE;
    lastModDate        : U16LE;
    crc32              : U32LE;
    compressedSize     : U32LE;
    uncompressedSize   : U32LE;
    fileNameLength     : U16LE;
    extraFieldLength   : U16LE;
    fileCommentLength  : U16LE;
    diskNumberStart    : U16LE;
    internalFileAttrs  : U16LE;
    externalFileAttrs  : U32LE;
    localHeaderOffset  : U32LE;
    fileName           : U8[fileNameLength];
    extraField         : U8[extraFieldLength];
    fileComment        : U8[fileCommentLength];
  }

  struct EndOfCentralDirectoryRecord {
    signature          : U32LE = 0x06054b50;
    diskNumber         : U16LE;
    diskStart          : U16LE;
    numEntriesThisDisk : U16LE;
    numEntries         : U16LE;
    centralDirectorySize : U32LE;
    centralDirectoryOffset : U32LE;
    commentLength      : U16LE;
    comment            : U8[commentLength];
  }

  struct ZIPFile {
    localFiles         : LocalFileHeader[*];
    centralDirectory   : CentralDirectoryFileHeader[*];
    endOfCentralDir    : EndOfCentralDirectoryRecord;
  }

  let zipParser = parse ZIPFile from BitStream;
}