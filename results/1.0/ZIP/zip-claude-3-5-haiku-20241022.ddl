define ZIP {
    enum CompressionMethod {
        STORE = 0,
        SHRINK = 1,
        REDUCE1 = 2,
        REDUCE2 = 3,
        REDUCE3 = 4,
        REDUCE4 = 5,
        IMPLODE = 6,
        TOKENIZE = 7,
        DEFLATE = 8,
        DEFLATE64 = 9,
        PKWARE_IMPLODE = 10,
        BZIP2 = 12,
        LZMA = 14,
        TERSE = 18,
        LZ77 = 19,
        WAVPACK = 97,
        PPMD = 98
    }

    bitflags BitFlag: u16 {
        ENCRYPTED = 0x01,
        COMPRESSION_OPTIONS = 0x02,
        COMPRESSED_PATCH = 0x04,
        STRONG_ENCRYPTION = 0x40
    }

    struct LocalFileHeader {
        signature: u32 = 0x04034b50,
        version_extract: u16,
        bit_flags: BitFlag,
        compression_method: CompressionMethod,
        last_mod_time: u16,
        last_mod_date: u16,
        crc32: u32,
        compressed_size: u32,
        uncompressed_size: u32,
        filename_length: u16,
        extra_field_length: u16,
        filename: string(filename_length),
        extra_field: bytes(extra_field_length)
    }

    struct DataDescriptor {
        crc32: u32 as optional,
        compressed_size: u32 as optional,
        uncompressed_size: u32 as optional
    }

    struct CentralDirectoryHeader {
        signature: u32 = 0x02014b50,
        version_made_by: u16,
        version_extract: u16,
        bit_flags: BitFlag,
        compression_method: CompressionMethod,
        last_mod_time: u16,
        last_mod_date: u16,
        crc32: u32,
        compressed_size: u32,
        uncompressed_size: u32,
        filename_length: u16,
        extra_field_length: u16,
        file_comment_length: u16,
        disk_number_start: u16,
        internal_file_attributes: u16,
        external_file_attributes: u32,
        local_header_offset: u32,
        filename: string(filename_length),
        extra_field: bytes(extra_field_length),
        file_comment: string(file_comment_length)
    }

    struct EndOfCentralDirectory {
        signature: u32 = 0x06054b50,
        disk_number: u16,
        central_directory_disk: u16,
        central_directory_entries_disk: u16,
        central_directory_total_entries: u16,
        central_directory_size: u32,
        central_directory_offset: u32,
        zip_comment_length: u16,
        zip_comment: string(zip_comment_length)
    }

    struct Zip64EndOfCentralDirectory {
        signature: u32 = 0x06064b50,
        size_of_record: u64,
        version_made_by: u16,
        version_extract: u16,
        disk_number: u32,
        central_directory_disk: u32,
        central_directory_entries_disk: u64,
        central_directory_total_entries: u64,
        central_directory_size: u64,
        central_directory_offset: u64
    }

    root: {
        local_file_headers: LocalFileHeader[],
        file_data: bytes[],
        optional_data_descriptors: DataDescriptor[] as optional,
        central_directory_headers: CentralDirectoryHeader[],
        end_of_central_directory: EndOfCentralDirectory,
        optional_zip64_end_of_central_directory: Zip64EndOfCentralDirectory as optional
    }
}