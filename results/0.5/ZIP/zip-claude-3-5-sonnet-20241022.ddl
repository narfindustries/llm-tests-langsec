def ZIP = {
    magic = $"PK\x03\x04";
    local_file_header* {
        signature = magic;
        version = uint16;
        flags = uint16;
        compression = uint16;
        mod_time = uint16;
        mod_date = uint16;
        crc32 = uint32;
        compressed_size = uint32;
        uncompressed_size = uint32;
        filename_length = uint16;
        extra_field_length = uint16;
        filename = byte[filename_length];
        extra_field = byte[extra_field_length];
        compressed_data = byte[compressed_size];
    }
    central_directory* {
        signature = $"PK\x01\x02";
        version_made_by = uint16;
        version_needed = uint16;
        flags = uint16;
        compression = uint16;
        mod_time = uint16;
        mod_date = uint16;
        crc32 = uint32;
        compressed_size = uint32;
        uncompressed_size = uint32;
        filename_length = uint16;
        extra_field_length = uint16;
        comment_length = uint16;
        disk_number_start = uint16;
        internal_attrs = uint16;
        external_attrs = uint32;
        local_header_offset = uint32;
        filename = byte[filename_length];
        extra_field = byte[extra_field_length];
        comment = byte[comment_length];
    }
    end_of_central_directory {
        signature = $"PK\x05\x06";
        disk_number = uint16;
        central_dir_disk = uint16;
        disk_entries = uint16;
        total_entries = uint16;
        central_dir_size = uint32;
        central_dir_offset = uint32;
        comment_length = uint16;
        comment = byte[comment_length];
    }
}