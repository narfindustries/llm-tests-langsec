Zip = {
    CompressionMethod = enum(U16) {
        Store = 0,
        Shrink = 1,
        Reduce1 = 2,
        Reduce2 = 3,
        Reduce3 = 4,
        Reduce4 = 5,
        Implode = 6,
        Deflate = 8,
        Deflate64 = 9,
        PKWAREImplode = 10,
        BZIP2 = 12,
        LZMA = 14,
        TERSE = 18,
        LZ77 = 19,
        WavPack = 97,
        PPMd = 98
    },

    LocalFileHeader = struct {
        signature: magic(0x04034B50),
        version_needed: U16,
        flags: U16,
        compression: CompressionMethod,
        last_mod_time: U16,
        last_mod_date: U16,
        crc32: U32,
        compressed_size: U32,
        uncompressed_size: U32,
        filename_length: U16,
        extra_field_length: U16,
        filename: string(filename_length),
        extra_field: bytes(extra_field_length),
        compressed_data: bytes(compressed_size)
    },

    CentralDirectoryFileHeader = struct {
        signature: magic(0x02014B50),
        version_made_by: U16,
        version_needed: U16,
        flags: U16,
        compression: CompressionMethod,
        last_mod_time: U16,
        last_mod_date: U16,
        crc32: U32,
        compressed_size: U32,
        uncompressed_size: U32,
        filename_length: U16,
        extra_field_length: U16,
        file_comment_length: U16,
        disk_number_start: U16,
        internal_file_attributes: U16,
        external_file_attributes: U32,
        local_header_offset: U32,
        filename: string(filename_length),
        extra_field: bytes(extra_field_length),
        file_comment: string(file_comment_length)
    },

    EndOfCentralDirectory = struct {
        signature: magic(0x06054B50),
        disk_number: U16,
        central_directory_disk: U16,
        central_directory_entries_on_disk: U16,
        total_central_directory_entries: U16,
        central_directory_size: U32,
        central_directory_offset: U32,
        comment_length: U16,
        comment: string(comment_length)
    },

    Zip64EndOfCentralDirectory = struct {
        signature: magic(0x06064B50),
        size_of_record: U64,
        version_made_by: U16,
        version_needed: U16,
        disk_number: U32,
        central_directory_disk: U32,
        central_directory_entries_on_disk: U64,
        total_central_directory_entries: U64,
        central_directory_size: U64,
        central_directory_offset: U64
    },

    ZipFile = struct {
        local_files: [LocalFileHeader],
        central_directory: [CentralDirectoryFileHeader],
        end_of_central_directory: EndOfCentralDirectory,
        zip64_eocdr: optional(Zip64EndOfCentralDirectory)
    }
}