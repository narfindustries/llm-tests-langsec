module ZIP {
    struct ZipFile {
        local file_count: u16 = 0;
        files: FileEntry[file_count];
    }

    struct FileEntry {
        local header_signature: u32 = 0x04034b50;
        local extra_field_length: u16 = 0;
        local file_name_length: u16 = 0;
        local compressed_size: u32 = 0;
        local uncompressed_size: u32 = 0;
        local compression_method: u16 = 0;
        local crc32: u32 = 0;
        local file_name: string[file_name_length];
        local extra_field: bytes[extra_field_length];
        local file_data: bytes[compressed_size];

        header: FileHeader = {
            signature: header_signature,
            version_needed: u16,
            flags: u16,
            compression_method: compression_method,
            last_mod_time: u16,
            last_mod_date: u16,
            crc32: crc32,
            compressed_size: compressed_size,
            uncompressed_size: uncompressed_size,
            file_name_length: file_name_length,
            extra_field_length: extra_field_length
        };

        struct FileHeader {
            signature: u32;
            version_needed: u16;
            flags: u16;
            compression_method: u16;
            last_mod_time: u16;
            last_mod_date: u16;
            crc32: u32;
            compressed_size: u32;
            uncompressed_size: u32;
            file_name_length: u16;
            extra_field_length: u16;
        }
    }
}