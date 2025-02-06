module ZIP;

type U2 = uint(16);
type U4 = uint(32);

// Parsing MS-DOS time and date
def parseDosTime(time: U2): (U2, U2, U2) = (
    (time >> 11) & 0x1F, // hours
    (time >> 5) & 0x3F,  // minutes
    (time & 0x1F) * 2    // seconds
);

def parseDosDate(date: U2): (U2, U2, U2) = (
    ((date >> 9) & 0x7F) + 1980, // year
    (date >> 5) & 0x0F,          // month
    date & 0x1F                  // day
);

// Local File Header
structure LocalFileHeader {
    U4 signature = 0x04034b50;
    U2 versionNeeded;
    U2 generalPurposeBits;
    U2 compressionMethod;
    U2 lastModTime;
    U2 lastModDate;
    U4 crc32;
    U4 compressedSize;
    U4 uncompressedSize;
    U2 fileNameLength;
    U2 extraFieldLength;
    byte[fileNameLength] filename;
    byte[extraFieldLength] extraField;
}

// Data Descriptor
structure DataDescriptor {
    U4 crc32 = if (@.generalPurposeBits & 0x0008) != 0 then input.u32le;
    U4 compressedSize = if (@.generalPurposeBits & 0x0008) != 0 then input.u32le;
    U4 uncompressedSize = if (@.generalPurposeBits & 0x0008) != 0 then input.u32le;
}

// Central Directory File Header
structure CentralDirectoryFileHeader {
    U4 signature = 0x02014b50;
    U2 versionMadeBy;
    U2 versionNeededToExtract;
    U2 generalPurposeBits;
    U2 compressionMethod;
    U2 lastModTime;
    U2 lastModDate;
    U4 crc32;
    U4 compressedSize;
    U4 uncompressedSize;
    U2 fileNameLength;
    U2 extraFieldLength;
    U2 fileCommentLength;
    U2 diskNumberStart;
    U2 internalFileAttributes;
    U4 externalFileAttributes;
    U4 offsetOfLocalHeader;
    byte[fileNameLength] filename;
    byte[extraFieldLength] extraField;
    byte[fileCommentLength] fileComment;
}

// End of Central Directory Record
structure EndOfCentralDirectoryRecord {
    U4 signature = 0x06054b50;
    U2 numberOfThisDisk;
    U2 diskWithCentralDirectory;
    U2 entriesOnThisDisk;
    U2 totalEntries;
    U4 sizeOfCentralDirectory;
    U4 offsetOfStartOfCentralDirectory;
    U2 zipFileCommentLength;
    byte[zipFileCommentLength] comment;
}

// Complete ZIP file structure
structure ZIPFile {
    repeat(LocalFileHeader) files;
    repeat(CentralDirectoryFileHeader) centralDirectories;
    EndOfCentralDirectoryRecord endOfCentralDirectory;
}