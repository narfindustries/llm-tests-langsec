ZIPFile ::= {
  local_file_headers: LocalFileHeader*,
  central_directory: CentralDirectory,
  end_of_central_directory: EndOfCentralDirectory,
  zip64_extensions: Optional<ZIP64Extensions>
}

LocalFileHeader ::= {
  signature: U32 = 0x04034b50,
  version_needed_to_extract: U16,
  general_purpose_bit_flag: U16,
  compression_method: U16,
  last_mod_file_time: U16,
  last_mod_file_date: U16,
  crc32: U32,
  compressed_size: U32,
  uncompressed_size: U32,
  file_name_length: U16,
  extra_field_length: U16,
  file_name: String(file_name_length),
  extra_field: Optional<ExtraField(extra_field_length)>
}

CentralDirectory ::= {
  central_file_headers: CentralFileHeader*
}

CentralFileHeader ::= {
  signature: U32 = 0x02014b50,
  version_made_by: U16,
  version_needed_to_extract: U16,
  general_purpose_bit_flag: U16,
  compression_method: U16,
  last_mod_file_time: U16,
  last_mod_file_date: U16,
  crc32: U32,
  compressed_size: U32,
  uncompressed_size: U32,
  file_name_length: U16,
  extra_field_length: U16,
  file_comment_length: U16,
  disk_number_start: U16,
  internal_file_attributes: U16,
  external_file_attributes: U32,
  relative_offset_of_local_header: U32,
  file_name: String(file_name_length),
  extra_field: Optional<ExtraField(extra_field_length)>,
  file_comment: Optional<String(file_comment_length)>
}

EndOfCentralDirectory ::= {
  signature: U32 = 0x06054b50,
  number_of_this_disk: U16,
  disk_with_start_of_central_directory: U16,
  number_of_central_dir_entries_on_this_disk: U16,
  total_number_of_central_dir_entries: U16,
  size_of_central_directory: U32,
  offset_of_start_of_central_directory: U32,
  zip_file_comment_length: U16,
  zip_file_comment: Optional<String(zip_file_comment_length)>
}

ZIP64Extensions ::= {
  zip64_end_of_central_dir_signature: U32 = 0x06064b50,
  size_of_zip64_end_of_central_dir_record: U64,
  version_made_by: U16,
  version_needed_to_extract: U16,
  number_of_this_disk: U32,
  disk_with_start_of_central_directory: U32,
  number_of_central_dir_entries_on_this_disk: U64,
  total_number_of_central_dir_entries: U64,
  size_of_central_directory: U64,
  offset_of_start_of_central_directory: U64,
  zip64_extensible_data_sector: Optional<Bytes>
}

ExtraField ::= (length: U16) {
  header_id: U16,
  data_size: U16,
  data: Bytes(data_size)
}

DataDescriptor ::= {
  signature: U32 = 0x08074b50,
  crc32: U32,
  compressed_size: U32,
  uncompressed_size: U32
}