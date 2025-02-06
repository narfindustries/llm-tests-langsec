local_file_header = struct {
    u4  signature;                  // 0x04034b50
    u2  version_needed;
    u2  general_purpose_bit_flag;
    u2  compression_method;
    u2  last_mod_file_time;
    u2  last_mod_file_date;
    u4  crc32;
    u4  compressed_size;
    u4  uncompressed_size;
    u2  filename_length;
    u2  extra_field_length;
    u1  filename[filename_length];
    u1  extra_field[extra_field_length];
};

central_directory_file_header = struct {
    u4  signature;                  // 0x02014b50
    u2  version_made_by;
    u2  version_needed;
    u2  general_purpose_bit_flag;
    u2  compression_method;
    u2  last_mod_file_time;
    u2  last_mod_file_date;
    u4  crc32;
    u4  compressed_size;
    u4  uncompressed_size;
    u2  filename_length;
    u2  extra_field_length;
    u2  file_comment_length;
    u2  disk_number_start;
    u2  internal_file_attrs;
    u4  external_file_attrs;
    u4  local_header_offset;
    u1  filename[filename_length];
    u1  extra_field[extra_field_length];
    u1  file_comment[file_comment_length];
};

end_of_central_directory_record = struct {
    u4  signature;                  // 0x06054b50
    u2  number_of_this_disk;
    u2  disk_with_start_of_central_directory;
    u2  total_entries_disk;
    u2  total_entries;
    u4  size_of_central_directory;
    u4  offset_of_central_directory;
    u2  zip_comment_length;
    u1  zip_comment[zip_comment_length];
};

data_descriptor = struct {
    u4  signature;                  // Optional, 0x08074b50
    u4  crc32;
    u4  compressed_size;
    u4  uncompressed_size;
} optional;

archive_extra_data_record = struct {
    u4  signature;                  // 0x08064b50
    u4  extra_length;
    u1  extra_data[extra_length];
} optional;

zip64_end_of_central_directory_record = struct {
    u4  signature;                  // 0x06064b50
    u8  size_of_zip64_end_record;
    u2  version_made_by;
    u2  version_needed;
    u4  number_of_this_disk;
    u4  disk_with_start_of_central_directory;
    u8  total_entries_disk;
    u8  total_entries;
    u8  size_of_central_directory;
    u8  offset_of_central_directory;
} optional;

zip64_end_of_central_directory_locator = struct {
    u4  signature;                  // 0x07064b50
    u4  number_of_disk_with_zip64_end;
    u8  offset_of_zip64_end;
    u4  total_number_of_disks;
} optional;

zip_file = struct {
    local_file_header local_headers[];
    central_directory_file_header central_directory[];
    data_descriptor data_descriptors[];
    archive_extra_data_record archive_extra_data;
    zip64_end_of_central_directory_record zip64_end_record;
    zip64_end_of_central_directory_locator zip64_locator;
    end_of_central_directory_record end_record;
};