domain ZIP {
  format name = " ZIP File Format"
  long_name = "ZIP file format"
  reference_id = 1
  version = 1.0

  # zip local file header
  structure local_file_header {
    field signature {
      type = uint16
      value = 0x04034b50
    }
    field version_needed {
      type = uint16
    }
    field flags {
      type = uint16
    }
    field compression {
      type = uint16
    }
    field last_mod_time {
      type = uint16
    }
    field last_mod_date {
      type = uint16
    }
    field crc32 {
      type = uint32
    }
    field compressed_size {
      type = uint32
    }
    field uncompressed_size {
      type = uint32
    }
    field filename_length {
      type = uint16
    }
    field extra_field_length {
      type = uint16
    }
    field filename {
      type = string
      length = filename_length
    }
    field extra_field {
      type = bytes
      length = extra_field_length
    }
  }

  # zip file descriptor
  structure file_descriptor {
    field signature {
      type = uint32
      value = 0x08074b50
    }
    field crc32 {
      type = uint32
    }
    field compressed_size {
      type = uint64
    }
    field uncompressed_size {
      type = uint64
    }
  }

  # zip central directory file header
  structure central_directory_file_header {
    field signature {
      type = uint32
      value = 0x02014b50
    }
    field version_made {
      type = uint16
    }
    field version_needed {
      type = uint16
    }
    field flags {
      type = uint16
    }
    field compression {
      type = uint16
    }
    field last_mod_time {
      type = uint16
    }
    field last_mod_date {
      type = uint16
    }
    field crc32 {
      type = uint32
    }
    field compressed_size {
      type = uint32
    }
    field uncompressed_size {
      type = uint32
    }
    field filename_length {
      type = uint16
    }
    field extra_field_length {
      type = uint16
    }
    field file_comment_length {
      type = uint16
    }
    field disk_number_start {
      type = uint16
    }
    field int_file_attr {
      type = uint16
    }
    field ext_file_attr {
      type = uint32
    }
    field local_header_offset {
      type = uint32
    }
    field filename {
      type = string
      length = filename_length
    }
    field extra_field {
      type = bytes
      length = extra_field_length
    }
    field file_comment {
      type = string
      length = file_comment_length
    }
  }

  # zip end of central directory
  structure end_of_central_directory {
    field signature {
      type = uint32
      value = 0x06054b50
    }
    field disk_number {
      type = uint16
    }
    field disk_number_start {
      type = uint16
    }
    field num_central_dir_entries {
      type = uint16
    }
    field total_central_dir_entries {
      type = uint16
    }
    field central_dir_size {
      type = uint32
    }
    field central_dir_offset {
      type = uint32
    }
    field comment_length {
      type = uint16
    }
    field comment {
      type = string
      length = comment_length
    }
  }
}