format little_endian {
  struct LocalFileHeader {
    local_file_header_signature: uint32 = 0x04034b50,
    version_needed_to_extract: uint16,
    general_purpose_bit_flag: uint16,
    compression_method: uint16,
    last_mod_file_time: uint16,
    last_mod_file_date: uint16,
    crc_32: uint32,
    compressed_size: uint32,
    uncompressed_size: uint32,
    file_name_length: uint16,
    extra_field_length: uint16,
  }

  struct DataDescriptor {
    data_descriptor_signature: uint32 = 0x08074b50,
    crc_32: uint32,
    compressed_size: uint32,
    uncompressed_size: uint32,
  }

  struct CentralDirectory {
    central_directory_header_signature: uint32 = 0x02014b50,
    version_made_by: uint16,
    version_needed_to_extract: uint16,
    general_purpose_bit_flag: uint16,
    compression_method: uint16,
    last_mod_file_time: uint16,
    last_mod_file_date: uint16,
    crc_32: uint32,
    compressed_size: uint32,
    uncompressed_size: uint32,
    file_name_length: uint16,
    extra_field_length: uint16,
    file_comment_length: uint16,
    disk_number_start: uint16,
    int_file_attributes: uint16,
    ext_file_attributes: uint32,
    local_header_offset: uint32,
  }

  struct EndOfCentralDirectory {
    end_of_central_directory_signature: uint32 = 0x06054b50,
    number_of_this_disk: uint16,
    number_of_the_disk_where_the_central_directory_starts: uint16,
    total_number_of_entries_in_the_central_directory_on_this_disk: uint16,
    total_number_of_entries_in_the_central_directory: uint16,
    size_of_the_central_directory: uint32,
    offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number: uint32,
    zipfile_comment_length: uint16,
  }

  struct ZipFile {
    local_file_headers: [LocalFileHeader],
    data_descriptors: [DataDescriptor],
    central_directories: [CentralDirectory],
    end_of_central_directory: EndOfCentralDirectory,
  }

  struct ZipFileHeader {
    local_file_header: LocalFileHeader,
    file_name: bytes[:local_file_header.file_name_length],
    extra_field: bytes[:local_file_header.extra_field_length],
    file_data: bytes[:local_file_header.compressed_size],
    data_descriptor: DataDescriptor,
  }

  struct ZipCentralDirectory {
    central_directory: CentralDirectory,
    file_name: bytes[:central_directory.file_name_length],
    extra_field: bytes[:central_directory.extra_field_length],
    file_comment: bytes[:central_directory.file_comment_length],
  }

  struct ZipEndOfCentralDirectory {
    end_of_central_directory: EndOfCentralDirectory,
    zipfile_comment: bytes[:end_of_central_directory.zipfile_comment_length],
  }

  root type ZipFile
}