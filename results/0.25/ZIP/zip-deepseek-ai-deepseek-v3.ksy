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
        size: file_name_length
        type: str
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
        type: extra_field
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
        size: file_name_length
        type: str
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
        type: extra_field
      - id: file_comment
        size: file_comment_length
        type: str
        encoding: UTF-8
  end_of_central_directory_record:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x05, 0x06]
      - id: number_of_disks
        type: u2
      - id: disk_number
        type: u2
      - id: number_of_entries_on_disk
        type: u2
      - id: total_number_of_entries
        type: u2
      - id: size_of_central_directory
        type: u4
      - id: offset_of_central_directory
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
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
  extra_field:
    seq:
      - id: header_id
        type: u2
      - id: data_size
        type: u2
      - id: data
        size: data_size
        type: str
        encoding: UTF-8