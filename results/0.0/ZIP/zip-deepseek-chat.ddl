struct ZIPFile {
    local_file_header: LocalFileHeader[];
    central_directory: CentralDirectoryHeader[];
    end_of_central_directory: EndOfCentralDirectoryRecord;
}

struct LocalFileHeader {
    signature: uint32 = 0x04034b50;
    version_needed_to_extract: uint16;
    general_purpose_bit_flag: uint16;
    compression_method: uint16;
    last_mod_file_time: uint16;
    last_mod_file_date: uint16;
    crc32: uint32;
    compressed_size: uint32;
    uncompressed_size: uint32;
    file_name_length: uint16;
    extra_field_length: uint16;
    file_name: string(file_name_length);
    extra_field: byte[extra_field_length];
    file_data: byte[compressed_size];
}

struct CentralDirectoryHeader {
    signature: uint32 = 0x02014b50;
    version_made_by: uint16;
    version_needed_to_extract: uint16;
    general_purpose_bit_flag: uint16;
    compression_method: uint16;
    last_mod_file_time: uint16;
    last_mod_file_date: uint16;
    crc32: uint32;
    compressed_size: uint32;
    uncompressed_size: uint32;
    file_name_length: uint16;
    extra_field_length: uint16;
    file_comment_length: uint16;
    disk_number_start: uint16;
    internal_file_attributes: uint16;
    external_file_attributes: uint32;
    relative_offset_of_local_header: uint32;
    file_name: string(file_name_length);
    extra_field: byte[extra_field_length];
    file_comment: string(file_comment_length);
}

struct EndOfCentralDirectoryRecord {
    signature: uint32 = 0x06054b50;
    number_of_this_disk: uint16;
    number_of_disk_with_start_of_central_directory: uint16;
    number_of_central_directory_entries_on_this_disk: uint16;
    total_number_of_central_directory_entries: uint16;
    size_of_central_directory: uint32;
    offset_of_start_of_central_directory: uint32;
    zip_file_comment_length: uint16;
    zip_file_comment: string(zip_file_comment_length);
}