module ZipFormat {
    type ZipFile = {
        local_file_headers: List<LocalFileHeader>,
        central_directory: List<CentralDirectoryFileHeader>,
        end_of_central_directory: EndOfCentralDirectoryRecord
    }

    type LocalFileHeader = {
        signature: u32 where value == 0x04034B50,
        version_needed: u16,
        general_purpose_bit_flag: BitFlags,
        compression_method: CompressionMethod,
        last_mod_time: u16,
        last_mod_date: u16,
        crc32: u32,
        compressed_size: u32,
        uncompressed_size: u32,
        filename_length: u16,
        extra_field_length: u16,
        filename: String(filename_length),
        extra_field: Optional<ExtraField>
    }

    type BitFlags = {
        encryption_flag: bool,
        compression_option1: bool,
        compression_option2: bool,
        data_descriptor_flag: bool,
        language_encoding_flag: bool
    }

    enum CompressionMethod {
        NoCompression = 0,
        Shrink = 1,
        ReduceFactor1 = 2,
        ReduceFactor2 = 3,
        ReduceFactor3 = 4,
        ReduceFactor4 = 5,
        Implode = 6,
        Deflate = 8,
        Deflate64 = 9,
        BZIP2 = 12,
        LZMA = 14,
        IBMTerse = 18,
        LZ77 = 19,
        Zstandard = 98,
        MP3 = 99
    }

    type CentralDirectoryFileHeader = {
        signature: u32 where value == 0x02014B50,
        version_made_by: u16,
        version_needed: u16,
        general_purpose_bit_flag: BitFlags,
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
        relative_offset_local_header: u32,
        filename: String(filename_length),
        extra_field: Optional<ExtraField>,
        file_comment: Optional<String>
    }

    type EndOfCentralDirectoryRecord = {
        signature: u32 where value == 0x06054B50,
        number_of_this_disk: u16,
        disk_with_central_directory: u16,
        total_entries_this_disk: u16,
        total_entries_central_directory: u16,
        size_central_directory: u32,
        offset_central_directory: u32,
        zip_file_comment_length: u16,
        zip_file_comment: Optional<String>
    }

    type Zip64EndOfCentralDirectoryRecord = {
        signature: u32 where value == 0x06064B50,
        size_of_record: u64,
        version_made_by: u16,
        version_needed: u16,
        number_of_this_disk: u32,
        disk_with_central_directory: u32,
        total_entries_this_disk: u64,
        total_entries_central_directory: u64,
        size_central_directory: u64,
        offset_central_directory: u64
    }

    type ExtraField = {
        header_id: u16,
        data_size: u16,
        data: Bytes(data_size)
    }
}