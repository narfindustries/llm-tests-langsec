module ZIP {

  import DAEDALUS::BitStream;
  import DAEDALUS::Utils;

  struct LocalFileHeader {
    signature       : u32 = 0x04034b50;
    version         : u16;
    flags           : u16;
    compression     : u16;
    modTime         : u16;
    modDate         : u16;
    crc32           : u32;
    compressedSize  : u32;
    uncompressedSize: u32;
    fileNameLength  : u16;
    extraFieldLength: u16;
    fileName        : u8[fileNameLength];
    extraField      : u8[extraFieldLength];
  }

  struct CentralDirectoryFileHeader {
    signature       : u32 = 0x02014b50;
    versionMadeBy   : u16;
    versionNeeded   : u16;
    flags           : u16;
    compression     : u16;
    modTime         : u16;
    modDate         : u16;
    crc32           : u32;
    compressedSize  : u32;
    uncompressedSize: u32;
    fileNameLength  : u16;
    extraFieldLength: u16;
    commentLength   : u16;
    diskNumberStart : u16;
    internalAttributes : u16;
    externalAttributes : u32;
    localHeaderOffset  : u32;
    fileName           : u8[fileNameLength];
    extraField         : u8[extraFieldLength];
    fileComment        : u8[commentLength];
  }

  struct EndOfCentralDirectoryRecord {
    signature       : u32 = 0x06054b50;
    diskNumber      : u16;
    diskStart       : u16;
    numEntriesThisDisk: u16;
    numEntries      : u16;
    centralDirectorySize: u32;
    centralDirectoryOffset: u32;
    commentLength   : u16;
    comment         : u8[commentLength];
  }

  struct ZIPFile {
    files           : LocalFileHeader[+];
    centralDirectory: CentralDirectoryFileHeader[+];
    endOfDirectory  : EndOfCentralDirectoryRecord;
  }
}