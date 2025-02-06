module ZIP;

type ZipFile = struct {
    files : LocalFileHeader[];
    centralDirectories : CentralDirectory[];
    endOfCentralDirectory : EndOfCentralDirectory;
};

type LocalFileHeader = struct {
    signature : u32 = 0x04034b50;
    versionNeededToExtract : u16;
    generalPurposeBitFlag : u16;
    compressionMethod : u16;
    lastModFileTime : u16;
    lastModFileDate : u16;
    crc32 : u32;
    compressedSize : u32;
    uncompressedSize : u32;
    fileNameLength : u16;
    extraFieldLength : u16;
    fileName : bytes[fileNameLength];
    extraField : bytes[extraFieldLength];
    compressedData : bytes[compressedSize];
};

type CentralDirectory = struct {
    signature : u32 = 0x02014b50;
    versionMadeBy : u16;
    versionNeededToExtract : u16;
    generalPurposeBitFlag : u16;
    compressionMethod : u16;
    lastModFileTime : u16;
    lastModFileDate : u16;
    crc32 : u32;
    compressedSize : u32;
    uncompressedSize : u32;
    fileNameLength : u16;
    extraFieldLength : u16;
    fileCommentLength : u16;
    diskNumberStart : u16;
    internalFileAttributes : u16;
    externalFileAttributes : u32;
    relativeOffsetOfLocalHeader : u32;
    fileName : bytes[fileNameLength];
    extraField : bytes[extraFieldLength];
    fileComment : bytes[fileCommentLength];
};

type EndOfCentralDirectory = struct {
    signature : u32 = 0x06054b50;
    numberOfThisDisk : u16;
    diskWhereCentralDirectoryStarts : u16;
    numberOfCentralDirectoryRecordsOnThisDisk : u16;
    totalNumberOfCentralDirectoryRecords : u16;
    sizeOfCentralDirectory : u32;
    offsetOfStartOfCentralDirectory : u32;
    zipFileCommentLength : u16;
    zipFileComment : bytes[zipFileCommentLength];
};