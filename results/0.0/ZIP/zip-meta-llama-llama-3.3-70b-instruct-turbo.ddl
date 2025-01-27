domain zip {
  type zip_file {
    uint16 local_file_header_signature: 0x04034b50;
    uint16 version_needed_to_extract: 20;
    uint16 general_purpose_bit_flag: 0;
    uint16 compression_method: 0;
    uint16 last_modification_time: 0;
    uint16 last_modification_date: 0;
    uint32 crc32: 0;
    uint32 compressed_size: 0;
    uint32 uncompressed_size: 0;
    uint16 file_name_length: 0;
    uint16 extra_field_length: 0;
    byte[file_name_length] file_name;
    byte[extra_field_length] extra_field;
  }

  type zip_central_directory {
    uint32 signature: 0x02014b50;
    uint16 version_made_by: 45;
    uint16 version_needed_to_extract: 20;
    uint16 general_purpose_bit_flag: 0;
    uint16 compression_method: 0;
    uint16 last_modification_time: 0;
    uint16 last_modification_date: 0;
    uint32 crc32: 0;
    uint32 compressed_size: 0;
    uint32 uncompressed_size: 0;
    uint16 file_name_length: 0;
    uint16 extra_field_length: 0;
    uint16 file_comment_length: 0;
    uint16 disk_number_start: 0;
    uint16 internal_file_attributes: 0;
    uint32 external_file_attributes: 0;
    uint32 local_header_offset: 0;
    byte[file_name_length] file_name;
    byte[extra_field_length] extra_field;
    byte[file_comment_length] file_comment;
  }

  type zip_end_of_central_directory {
    uint32 signature: 0x06054b50;
    uint16 disk_number: 0;
    uint16 central_directory_disk_number: 0;
    uint16 central_directory_entries_on_disk: 0;
    uint16 central_directory_entries_total: 0;
    uint32 central_directory_size: 0;
    uint32 central_directory_offset: 0;
    uint16 comment_length: 0;
    byte[comment_length] comment;
  }

  grammar zip_file_grammar {
    rule file: local_file_header data_descriptor central_directory end_of_central_directory;
    rule local_file_header: local_file_header_signature version_needed_to_extract general_purpose_bit_flag compression_method last_modification_time last_modification_date crc32 compressed_size uncompressed_size file_name_length extra_field_length file_name extra_field;
    rule data_descriptor: crc32 compressed_size uncompressed_size;
    rule central_directory: central_directory_signature version_made_by version_needed_to_extract general_purpose_bit_flag compression_method last_modification_time last_modification_date crc32 compressed_size uncompressed_size file_name_length extra_field_length file_comment_length disk_number_start internal_file_attributes external_file_attributes local_header_offset file_name extra_field file_comment;
    rule end_of_central_directory: signature disk_number central_directory_disk_number central_directory_entries_on_disk central_directory_entries_total central_directory_size central_directory_offset comment_length comment;
  }
}