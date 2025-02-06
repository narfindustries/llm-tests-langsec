zip_file : struct {
    local_file_headers : local_file_header[];
    central_directory : central_directory_header[];
    end_of_central_directory : end_of_central_directory_record;
}

local_file_header : struct {
    signature : u32 = 0x04034b50;
    version_needed_to_extract : u16;
    general_purpose_bit_flag : u16;
    compression_method : u16;
    last_mod_file_time : u16;
    last_mod_file_date : u16;
    crc32 : u32;
    compressed_size : u32;
    uncompressed_size : u32;
    file_name_length : u16;
    extra_field_length : u16;
    file_name : string(file_name_length);
    extra_field : bytes(extra_field_length);
}

central_directory_header : struct {
    signature : u32 = 0x02014b50;
    version_made_by : u16;
    version_needed_to_extract : u16;
    general_purpose_bit_flag : u16;
    compression_method : u16;
    last_mod_file_time : u16;
    last_mod_file_date : u16;
    crc32 : u32;
    compressed_size : u32;
    uncompressed_size : u32;
    file_name_length : u16;
    extra_field_length : u16;
    file_comment_length : u16;
    disk_number_start : u16;
    internal_file_attributes : u16;
    external_file_attributes : u32;
    relative_offset_of_local_header : u32;
    file_name : string(file_name_length);
    extra_field : bytes(extra_field_length);
    file_comment : string(file_comment_length);
}

end_of_central_directory_record : struct {
    signature : u32 = 0x06054b50;
    number_of_disks : u16;
    disk_where_central_directory_starts : u16;
    number_of_central_directory_records_on_this_disk : u16;
    total_number_of_central_directory_records : u16;
    size_of_central_directory : u32;
    offset_of_start_of_central_directory : u32;
    zip_file_comment_length : u16;
    zip_file_comment : string(zip_file_comment_length);
}