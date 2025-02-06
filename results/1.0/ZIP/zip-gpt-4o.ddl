ZIP : Struct {
    files : Array(LocalFileHeader) [ size = end_of_central_directory.num_entries_central_dir ];

    central_directory : Array(CentralDirectoryFileHeader) [ size = end_of_central_directory.num_entries_central_dir ];

    end_of_central_directory : EndOfCentralDirectoryRecord;
}

LocalFileHeader : Struct {
    signature : UInt32 [ assert = 0x04034b50 ];
    version_needed_to_extract : UInt16;
    general_purpose_bit_flag : UInt16;
    compression_method : UInt16;
    last_mod_file_time : UInt16;
    last_mod_file_date : UInt16;
    crc32 : UInt32;
    compressed_size : UInt32;
    uncompressed_size : UInt32;
    file_name_length : UInt16;
    extra_field_length : UInt16;
    file_name : Bytes [ size = file_name_length ];
    extra_field : Bytes [ size = extra_field_length ];
    file_data : Bytes [ size = compressed_size ];

    optional_data_descriptor : Optional(OptionalDataDescriptor) [ if = (general_purpose_bit_flag & 0x0008) != 0 ];
}

OptionalDataDescriptor : Struct {
    crc32 : UInt32;
    compressed_size : UInt32;
    uncompressed_size : UInt32;
}

CentralDirectoryFileHeader : Struct {
    signature : UInt32 [ assert = 0x02014b50 ];
    version_made_by : UInt16;
    version_needed_to_extract : UInt16;
    general_purpose_bit_flag : UInt16;
    compression_method : UInt16;
    last_mod_file_time : UInt16;
    last_mod_file_date : UInt16;
    crc32 : UInt32;
    compressed_size : UInt32;
    uncompressed_size : UInt32;
    file_name_length : UInt16;
    extra_field_length : UInt16;
    file_comment_length : UInt16;
    disk_number_start : UInt16;
    internal_file_attributes : UInt16;
    external_file_attributes : UInt32;
    relative_offset_of_local_header : UInt32;
    file_name : Bytes [ size = file_name_length ];
    extra_field : Bytes [ size = extra_field_length ];
    file_comment : Bytes [ size = file_comment_length ];
}

EndOfCentralDirectoryRecord : Struct {
    signature : UInt32 [ assert = 0x06054b50 ];
    number_of_this_disk : UInt16;
    central_directory_start_disk : UInt16;
    num_entries_central_dir_on_disk : UInt16;
    num_entries_central_dir : UInt16;
    central_dir_size : UInt32;
    central_dir_offset : UInt32;
    zip_file_comment_length : UInt16;
    zip_file_comment : Bytes [ size = zip_file_comment_length ];
}