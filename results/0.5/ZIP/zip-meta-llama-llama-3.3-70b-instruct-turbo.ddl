format zip {
  local_file_header {
    signature: uint16 = 0x04034b50,
    version_needed_to_extract: uint16,
    general_purpose_bit_flag: uint16,
    compression_method: uint16,
    last_modified_time: uint16,
    last_modified_date: uint16,
    crc32: uint32,
    compressed_size: uint32,
    uncompressed_size: uint32,
    filename_length: uint16,
    extra_field_length: uint16,
    filename: byte[filename_length],
    extra_field: byte[extra_field_length]
  }

  extra_field {
    header_id: uint16,
    data_size: uint16,
    data: byte[data_size]
  }

  central_directory {
    signature: uint32 = 0x02014b50,
    version_made_by: uint16,
    version_needed_to_extract: uint16,
    general_purpose_bit_flag: uint16,
    compression_method: uint16,
    last_modified_time: uint16,
    last_modified_date: uint16,
    crc32: uint32,
    compressed_size: uint32,
    uncompressed_size: uint32,
    filename_length: uint16,
    extra_field_length: uint16,
    file_comment_length: uint16,
    disk_number_start: uint16,
    internal_attributes: uint16,
    external_attributes: uint32,
    local_header_offset: uint32,
    filename: byte[filename_length],
    extra_field: byte[extra_field_length],
    file_comment: byte[file_comment_length]
  }

  end_of_central_directory {
    signature: uint32 = 0x06054b50,
    number_of_this_disk: uint16,
    number_of_the_disk_where_the_central_directory_starts: uint16,
    number_of_entries_in_the_central_directory_on_this_disk: uint16,
    total_number_of_entries_in_the_central_directory: uint16,
    size_of_the_central_directory: uint32,
    offset_of_the_start_of_the_central_directory: uint32
  }

  zip_file {
    local_file_headers: local_file_header[*],
    central_directory: central_directory[*],
    eocd: end_of_central_directory
  }
}