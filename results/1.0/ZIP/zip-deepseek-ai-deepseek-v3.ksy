meta:
  id: zip
  title: PKWARE ZIP File Format
  license: CC0-1.0
  endian: le
seq:
  - id: local_file_headers
    type: local_file_header
    repeat: until
    repeat-until: _.signature == 0x04034b50
  - id: central_directory_headers
    type: central_directory_file_header
    repeat: until
    repeat-until: _.signature == 0x02014b50
  - id: end_of_central_directory
    type: end_of_central_directory
instances:
  central_directory_offset:
    value: end_of_central_directory.offset_of_cd_start
types:
  local_file_header:
    seq:
      - id: signature
        type: u4
        valid: 0x04034b50
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
        type: extra_field_data
  central_directory_file_header:
    seq:
      - id: signature
        type: u4
        valid: 0x02014b50
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
      - id: relative_offset_of_lfh
        type: u4
      - id: file_name
        size: file_name_length
        type: str
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
        type: extra_field_data
      - id: file_comment
        size: file_comment_length
        type: str
        encoding: UTF-8
  end_of_central_directory:
    seq:
      - id: signature
        type: u4
        valid: 0x06054b50
      - id: number_of_disks
        type: u2
      - id: disk_number_of_cd_start
        type: u2
      - id: number_of_cd_entries
        type: u2
      - id: total_number_of_cd_entries
        type: u2
      - id: size_of_cd
        type: u4
      - id: offset_of_cd_start
        type: u4
      - id: zip_file_comment_length
        type: u2
      - id: zip_file_comment
        size: zip_file_comment_length
        type: str
        encoding: UTF-8
  extra_field_data:
    seq:
      - id: header_id
        type: u2
      - id: data_size
        type: u2
      - id: data
        size: data_size
        type: str
        encoding: UTF-8