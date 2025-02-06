ZIP
{
    local_file_header: LocalFileHeader;
    file_data: Bytes[local_file_header.CompressedSize];
    data_descriptor: Optional<DataDescriptor>;
    central_directory: CentralDirectory;
    end_of_central_directory: EndOfCentralDirectoryRecord;

    LocalFileHeader
    {
        Signature: UInt32 = 0x04034b50;
        VersionNeededToExtract: UInt16;
        GeneralPurposeBitFlag: UInt16;
        CompressionMethod: UInt16;
        LastModFileTime: UInt16;
        LastModFileDate: UInt16;
        CRC32: UInt32;
        CompressedSize: UInt32;
        UncompressedSize: UInt32;
        FileNameLength: UInt16;
        ExtraFieldLength: UInt16;
        FileName: String[FileNameLength];
        ExtraField: Bytes[ExtraFieldLength];
    };

    DataDescriptor
    {
        Signature: UInt32 = 0x08074b50;
        CRC32: UInt32;
        CompressedSize: UInt32;
        UncompressedSize: UInt32;
    };

    CentralDirectory
    {
        Signature: UInt32 = 0x02014b50;
        VersionMadeBy: UInt16;
        VersionNeededToExtract: UInt16;
        GeneralPurposeBitFlag: UInt16;
        CompressionMethod: UInt16;
        LastModFileTime: UInt16;
        LastModFileDate: UInt16;
        CRC32: UInt32;
        CompressedSize: UInt32;
        UncompressedSize: UInt32;
        FileNameLength: UInt16;
        ExtraFieldLength: UInt16;
        FileCommentLength: UInt16;
        DiskNumberStart: UInt16;
        InternalFileAttributes: UInt16;
        ExternalFileAttributes: UInt32;
        RelativeOffsetOfLocalHeader: UInt32;
        FileName: String[FileNameLength];
        ExtraField: Bytes[ExtraFieldLength];
        FileComment: String[FileCommentLength];
    };

    EndOfCentralDirectoryRecord
    {
        Signature: UInt32 = 0x06054b50;
        NumberOfDisks: UInt16;
        DiskWithCentralDirectoryStart: UInt16;
        NumberOfCentralDirectoryEntriesOnDisk: UInt16;
        TotalNumberOfCentralDirectoryEntries: UInt16;
        SizeOfCentralDirectory: UInt32;
        OffsetOfCentralDirectoryStart: UInt32;
        ZIPFileCommentLength: UInt16;
        ZIPFileComment: String[ZIPFileCommentLength];
    };
};