meta:
  id: zip
  file-extension: zip
  endian: le
seq:
  - id: local_file_headers
    type: local_file_header
    repeat: eos
types:
  local_file_header:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x03, 0x04]
      - id: version_needed_to_extract
        type: u2
      - id: general_purpose_bit_flag
        type: u2
      - id: compression_method
        type: u2
      - id: last_mod_file_time
        type: u2
      - id: last_mod_file_date
        type: u2
      - id: crc_32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
      - id: file_name_length
        type: u2
      - id: extra_field_length
        type: u2
      - id: file_name
        size: file_name_length
        type: str
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
  central_directory_file_header:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x01, 0x02]
      - id: version_made_by
        type: u2
      - id: version_needed_to_extract
        type: u2
      - id: general_purpose_bit_flag
        type: u2
      - id: compression_method
        type: u2
      - id: last_mod_file_time
        type: u2
      - id: last_mod_file_date
        type: u2
      - id: crc_32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
      - id: file_name_length
        type: u2
      - id: extra_field_length
        type: u2
      - id: file_comment_length
        type: u2
      - id: disk_number_start
        type: u2
      - id: internal_file_attributes
        type: u2
      - id: external_file_attributes
        type: u4
      - id: relative_offset_of_local_header
        type: u4
      - id: file_name
        size: file_name_length
        type: str
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
      - id: file_comment
        size: file_comment_length
        type: str
        encoding: UTF-8
  end_of_central_directory_record:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x05, 0x06]
      - id: disk_number
        type: u2
      - id: disk_number_with_start_of_central_directory
        type: u2
      - id: number_of_central_directory_records_on_this_disk
        type: u2
      - id: total_number_of_central_directory_records
        type: u2
      - id: size_of_central_directory
        type: u4
      - id: offset_of_start_of_central_directory
        type: u4
      - id: zip_file_comment_length
        type: u2
      - id: zip_file_comment
        size: zip_file_comment_length
        type: str
        encoding: UTF-8
  data_descriptor:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x07, 0x08]
      - id: crc_32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
  zip64_end_of_central_directory_record:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x06, 0x06]
      - id: size_of_zip64_end_of_central_directory_record
        type: u8
      - id: version_made_by
        type: u2
      - id: version_needed_to_extract
        type: u2
      - id: disk_number
        type: u4
      - id: disk_number_with_start_of_central_directory
        type: u4
      - id: number_of_central_directory_records_on_this_disk
        type: u8
      - id: total_number_of_central_directory_records
        type: u8
      - id: size_of_central_directory
        type: u8
      - id: offset_of_start_of_central_directory
        type: u8
  zip64_end_of_central_directory_locator:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x06, 0x07]
      - id: disk_number_with_start_of_zip64_end_of_central_directory_record
        type: u4
      - id: relative_offset_of_zip64_end_of_central_directory_record
        type: u8
      - id: total_number_of_disks
        type: u4