def Main = ZIP

def ZIP = {
  local_file_header*
  central_directory_header*
  end_of_central_directory
}

def local_file_header = {
  signature: 0x04034b50
  version_needed: uint16
  flags: uint16
  compression_method: uint16
  last_mod_time: uint16
  last_mod_date: uint16
  crc32: uint32
  compressed_size: uint32
  uncompressed_size: uint32
  file_name_length: uint16
  extra_field_length: uint16
  file_name: uint8[file_name_length]
  extra_field: uint8[extra_field_length]
  compressed_data: uint8[compressed_size]
}

def central_directory_header = {
  signature: 0x02014b50
  version_made_by: uint16
  version_needed: uint16
  flags: uint16
  compression_method: uint16
  last_mod_time: uint16
  last_mod_date: uint16
  crc32: uint32
  compressed_size: uint32
  uncompressed_size: uint32
  file_name_length: uint16
  extra_field_length: uint16
  file_comment_length: uint16
  disk_number_start: uint16
  internal_file_attributes: uint16
  external_file_attributes: uint32
  relative_offset: uint32
  file_name: uint8[file_name_length]
  extra_field: uint8[extra_field_length]
  file_comment: uint8[file_comment_length]
}

def end_of_central_directory = {
  signature: 0x06054b50
  disk_number: uint16
  start_disk_number: uint16
  total_entries_disk: uint16
  total_entries: uint16
  central_directory_size: uint32
  central_directory_offset: uint32
  comment_length: uint16
  comment: uint8[comment_length]
}