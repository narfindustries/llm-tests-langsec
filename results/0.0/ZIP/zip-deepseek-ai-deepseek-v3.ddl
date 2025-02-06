ZIPFile = {
  local_files: LocalFileHeader*,
  central_directory: CentralDirectoryHeader*,
  end_of_central_directory: EndOfCentralDirectoryRecord,
  zip64_end_of_central_directory: Optional<ZIP64EndOfCentralDirectoryRecord>,
  zip64_end_of_central_directory_locator: Optional<ZIP64EndOfCentralDirectoryLocator>
};

LocalFileHeader = {
  signature: U32 = 0x04034b50,
  version_needed: U16,
  general_purpose: U16,
  compression_method: U16,
  last_mod_time: U16,
  last_mod_date: U16,
  crc32: U32,
  compressed_size: U32,
  uncompressed_size: U32,
  file_name_length: U16,
  extra_field_length: U16,
  file_name: String(file_name_length),
  extra_field: Bytes(extra_field_length)
};

CentralDirectoryHeader = {
  signature: U32 = 0x02014b50,
  version_made_by: U16,
  version_needed: U16,
  general_purpose: U16,
  compression_method: U16,
  last_mod_time: U16,
  last_mod_date: U16,
  crc32: U32,
  compressed_size: U32,
  uncompressed_size: U32,
  file_name_length: U16,
  extra_field_length: U16,
  file_comment_length: U16,
  disk_number_start: U16,
  internal_attributes: U16,
  external_attributes: U32,
  relative_offset: U32,
  file_name: String(file_name_length),
  extra_field: Bytes(extra_field_length),
  file_comment: String(file_comment_length)
};

EndOfCentralDirectoryRecord = {
  signature: U32 = 0x06054b50,
  disk_number: U16,
  start_disk: U16,
  entries_on_disk: U16,
  total_entries: U16,
  central_dir_size: U32,
  central_dir_offset: U32,
  comment_length: U16,
  comment: String(comment_length)
};

ZIP64EndOfCentralDirectoryRecord = {
  signature: U32 = 0x06064b50,
  size_of_record: U64,
  version_made_by: U16,
  version_needed: U16,
  disk_number: U32,
  start_disk: U32,
  entries_on_disk: U64,
  total_entries: U64,
  central_dir_size: U64,
  central_dir_offset: U64
};

ZIP64EndOfCentralDirectoryLocator = {
  signature: U32 = 0x07064b50,
  start_disk: U32,
  central_dir_offset: U64,
  total_disks: U32
};

DataDescriptor = {
  signature: Optional<U32> = 0x08074b50,
  crc32: U32,
  compressed_size: U32,
  uncompressed_size: U32
};

ExtraField = {
  id: U16,
  size: U16,
  data: Bytes(size)
};

Optional<T> = T | None;