meta:
  id: zip
  file-extension: zip
  endian: le
types:
  local_file_header:
    seq:
      - id: signature
        size: 4
      - id: version_needed_to_extract
        size: 2
      - id: flags
        size: 2
      - id: compression_method
        size: 2
      - id: last_modified_time
        size: 2
      - id: last_modified_date
        size: 2
      - id: crc_32
        size: 4
      - id: compressed_size
        size: 4
      - id: uncompressed_size
        size: 4
      - id: file_name_length
        size: 2
      - id: extra_field_length
        size: 2
      - id: file_name
        size: file_name_length
        encoding: ascii
      - id: extra_field
        size: extra_field_length
        type: extra_field
  extra_field:
    seq:
      - id: header_id
        size: 2
      - id: data_size
        size: 2
      - id: data
        size: data_size
  data_descriptor:
    seq:
      - id: signature
        size: 4
      - id: crc_32
        size: 4
      - id: compressed_size
        size: 4
      - id: uncompressed_size
        size: 4
  central_directory:
    seq:
      - id: signature
        size: 4
      - id: version_made_by
        size: 2
      - id: version_to_extract
        size: 2
      - id: flags
        size: 2
      - id: compression_method
        size: 2
      - id: last_modified_time
        size: 2
      - id: last_modified_date
        size: 2
      - id: crc_32
        size: 4
      - id: compressed_size
        size: 4
      - id: uncompressed_size
        size: 4
      - id: file_name_length
        size: 2
      - id: extra_field_length
        size: 2
      - id: file_comment_length
        size: 2
      - id: disk_number_start
        size: 2
      - id: internal_file_attributes
        size: 2
      - id: external_file_attributes
        size: 4
      - id: local_header_offset
        size: 4
      - id: file_name
        size: file_name_length
        encoding: ascii
      - id: extra_field
        size: extra_field_length
        type: extra_field
      - id: file_comment
        size: file_comment_length
        encoding: ascii
  end_of_central_directory:
    seq:
      - id: signature
        size: 4
      - id: disk_number_this_disk
        size: 2
      - id: disk_number_start
        size: 2
      - id: central_directory_entries_on_this_disk
        size: 2
      - id: central_directory_entries_total
        size: 2
      - id: central_directory_size
        size: 4
      - id: central_directory_offset
        size: 4
      - id: zip_file_comment_length
        size: 2
      - id: zip_file_comment
        size: zip_file_comment_length
        encoding: ascii
seq:
  - id: local_file_header
    type: local_file_header
  - id: file_data
    size: local_file_header.compressed_size
    process: xz
  - id: data_descriptor
    type: data_descriptor
    if: local_file_header.flags & 8 != 0
  - id: central_directories
    type: central_directory
    repeat: eos
  - id: end_of_central_directory
    type: end_of_central_directory