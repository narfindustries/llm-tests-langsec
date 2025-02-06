ZipFile : struct {
    local_file_headers : LocalFileHeader[],
    central_directory_file_headers : CentralDirectoryFileHeader[],
    end_of_central_directory_record : EndOfCentralDirectoryRecord,
    zip64_end_of_central_directory_record? : Zip64EndOfCentralDirectoryRecord,
    zip64_end_of_central_directory_locator? : Zip64EndOfCentralDirectoryLocator,
}

LocalFileHeader : struct {
    signature : u32,  # 0x04034b50
    version_needed_to_extract : u16,
    general_purpose_bit_flag : u16,
    compression_method : u16,
    last_mod_file_time : u16,
    last_mod_file_date : u16,
    crc_32 : u32,
    compressed_size : u32,
    uncompressed_size : u32,
    file_name_length : u16,
    extra_field_length : u16,
    file_name : u8[file_name_length],
    extra_field : u8[extra_field_length],
}

CentralDirectoryFileHeader : struct {
    signature : u32,  # 0x02014b50
    version_made_by : u16,
    version_needed_to_extract : u16,
    general_purpose_bit_flag : u16,
    compression_method : u16,
    last_mod_file_time : u16,
    last_mod_file_date : u16,
    crc_32 : u32,
    compressed_size : u32,
    uncompressed_size : u32,
    file_name_length : u16,
    extra_field_length : u16,
    file_comment_length : u16,
    disk_number_start : u16,
    internal_file_attributes : u16,
    external_file_attributes : u32,
    relative_offset_of_local_header : u32,
    file_name : u8[file_name_length],
    extra_field : u8[extra_field_length],
    file_comment : u8[file_comment_length],
}

EndOfCentralDirectoryRecord : struct {
    signature : u32,  # 0x06054b50
    number_of_this_disk : u16,
    disk_where_central_directory_starts : u16,
    number_of_central_directory_records_on_this_disk : u16,
    total_number_of_central_directory_records : u16,
    size_of_central_directory : u32,
    offset_of_start_of_central_directory : u32,
    zip_file_comment_length : u16,
    zip_file_comment : u8[zip_file_comment_length],
}

Zip64EndOfCentralDirectoryRecord : struct {
    signature : u32,  # 0x06064b50
    size_of_zip64_end_of_central_directory_record : u64,
    version_made_by : u16,
    version_needed_to_extract : u16,
    number_of_this_disk : u32,
    disk_where_central_directory_starts : u32,
    number_of_central_directory_records_on_this_disk : u64,
    total_number_of_central_directory_records : u64,
    size_of_central_directory : u64,
    offset_of_start_of_central_directory : u64,
    extensible_data_sector : u8[],
}

Zip64EndOfCentralDirectoryLocator : struct {
    signature : u32,  # 0x07064b50
    disk_with_start_of_zip64_end_of_central_directory : u32,
    relative_offset_of_zip64_end_of_central_directory_record : u64,
    total_number_of_disks : u32,
}

DataDescriptor : struct {
    crc_32 : u32,
    compressed_size : u32,
    uncompressed_size : u32,
}