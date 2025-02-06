ZIPFile = {
  local_headers: LocalFileHeader[],
  central_directory: CentralDirectoryHeader[],
  end_of_central_directory: EndOfCentralDirectoryRecord
};

LocalFileHeader = {
  signature: U32BE = 0x04034b50,
  version_needed: U16BE,
  general_purpose_bit: U16BE,
  compression_method: CompressionMethod,
  last_mod_time: U16BE,
  last_mod_date: U16BE,
  crc32: U32BE,
  compressed_size: U32BE,
  uncompressed_size: U32BE,
  file_name_length: U16BE,
  extra_field_length: U16BE,
  file_name: String(file_name_length),
  extra_field: Bytes(extra_field_length)
};

CentralDirectoryHeader = {
  signature: U32BE = 0x02014b50,
  version_made_by: U16BE,
  version_needed: U16BE,
  general_purpose_bit: U16BE,
  compression_method: CompressionMethod,
  last_mod_time: U16BE,
  last_mod_date: U16BE,
  crc32: U32BE,
  compressed_size: U32BE,
  uncompressed_size: U32BE,
  file_name_length: U16BE,
  extra_field_length: U16BE,
  file_comment_length: U16BE,
  disk_number_start: U16BE,
  internal_attributes: U16BE,
  external_attributes: U32BE,
  relative_offset: U32BE,
  file_name: String(file_name_length),
  extra_field: Bytes(extra_field_length),
  file_comment: String(file_comment_length)
};

EndOfCentralDirectoryRecord = {
  signature: U32BE = 0x06054b50,
  disk_number: U16BE,
  disk_start: U16BE,
  num_entries_on_disk: U16BE,
  total_entries: U16BE,
  central_dir_size: U32BE,
  central_dir_offset: U32BE,
  comment_length: U16BE,
  comment: String(comment_length)
};

CompressionMethod = enum U16BE {
  STORED = 0,
  SHRUNK = 1,
  REDUCED_1 = 2,
  REDUCED_2 = 3,
  REDUCED_3 = 4,
  REDUCED_4 = 5,
  IMPLODED = 6,
  DEFLATED = 8,
  ENHANCED_DEFLATED = 9,
  PKWARE_DCL_IMPLODE = 10,
  BZIP2 = 12,
  LZMA = 14,
  IBM_TERSE = 18,
  IBM_LZ77_Z = 19,
  WAVPACK = 97,
  PPMD = 98
};

String(length: U16BE) = {
  value: Bytes(length)
};