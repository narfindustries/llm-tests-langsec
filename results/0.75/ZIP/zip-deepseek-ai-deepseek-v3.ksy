meta:
  id: zip
  file-extension: zip
  endian: le
  license: MIT
seq:
  - id: local_file_header
    type: local_file_header
  - id: file_data
    size: local_file_header.compressed_size
  - id: data_descriptor
    type: data_descriptor
    if: local_file_header.general_purpose_bit_flag & 0x0008 != 0
  - id: central_directory
    type: central_directory
    repeat: eos
  - id: end_of_central_directory
    type: end_of_central_directory
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
      - id: crc32
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
        type: str
        size: file_name_length
        encoding: ASCII
      - id: extra_field
        size: extra_field_length
  data_descriptor:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x07, 0x08]
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
  central_directory:
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
      - id: crc32
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
        type: str
        size: file_name_length
        encoding: ASCII
      - id: extra_field
        size: extra_field_length
      - id: file_comment
        type: str
        size: file_comment_length
        encoding: ASCII
  end_of_central_directory:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x05, 0x06]
      - id: number_of_this_disk
        type: u2
      - id: disk_where_central_directory_starts
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
        type: str
        size: zip_file_comment_length
        encoding: ASCII
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
      - id: number_of_this_disk
        type: u4
      - id: disk_where_central_directory_starts
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
      - id: disk_where_zip64_end_of_central_directory_record_starts
        type: u4
      - id: relative_offset_of_zip64_end_of_central_directory_record
        type: u8
      - id: total_number_of_disks
        type: u4