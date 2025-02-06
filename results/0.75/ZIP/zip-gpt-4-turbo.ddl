type u16le = U16
type u32le = U32

struct ZipDateTime {
    U16 year: 7,
    U16 month: 4,
    U16 day: 5,
    U16 hour: 5,
    U16 minute: 6,
    U16 second: 5
}

struct ZipGeneralPurposeBitFlag {
    U16 encrypted: 1,
    U16 compressionOption1: 1,
    U16 compressionOption2: 1,
    U16 dataDescriptor: 1,
    U16 enhancedDeflation: 1,
    U16 compressedPatchedData: 1,
    U16 strongEncryption: 1,
    U16 unused: 4,
    U16 langEncoding: 1,
    U16 reserved1: 1,
    U16 maskHeaderValues: 1,
    U16 reserved2: 2
}

struct ZipLocalFileHeader {
    U32 signature,
    u16le versionNeededToExtract,
    ZipGeneralPurposeBitFlag generalPurposeBitFlag,
    u16le compressionMethod,
    u16le lastModFileTime,
    u16le lastModFileDate,
    u32le crc32,
    u32le compressedSize,
    u32le uncompressedSize,
    u16le fileNameLength,
    u16le extraFieldLength,
    U8[fileNameLength] fileName,
    U8[extraFieldLength] extraField
}

struct ZipCentralDirectoryFileHeader {
    U32 signature,
    u16le versionMadeBy,
    u16le versionNeededToExtract,
    ZipGeneralPurposeBitFlag generalPurposeBitFlag,
    u16le compressionMethod,
    u16le lastModFileTime,
    u16le lastModFileDate,
    u32le crc32,
    u32le compressedSize,
    u32le uncompressedSize,
    u16le fileNameLength,
    u16le extraFieldLength,
    u16le fileCommentLength,
    u16le diskNumberStart,
    u16le internalFileAttributes,
    u32le externalFileAttributes,
    u32le offsetOfLocalHeader,
    U8[fileNameLength] fileName,
    U8[extraFieldLength] extraField,
    U8[fileCommentLength] fileComment
}

struct ZipEndOfCentralDirectoryRecord {
    U32 signature,
    u16le numberOfThisDisk,
    u16le diskWhereCentralDirectoryStarts,
    u16le numberOfCentralDirectoryRecordsOnThisDisk,
    u16le totalNumberOfCentralDirectoryRecords,
    u32le sizeOfCentralDirectory,
    u32le offsetOfStartOfCentralDirectory,
    u16le zipFileCommentLength,
    U8[zipFileCommentLength] zipFileComment
}

struct ZipFile {
    [ZipLocalFileHeader] localFileHeaders,
    [ZipCentralDirectoryFileHeader] centralDirectoryHeaders,
    ZipEndOfCentralDirectoryRecord endOfCentralDirectory
}