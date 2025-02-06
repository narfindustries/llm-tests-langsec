meta:
  id: zip
  file-extension: zip
  endian: le
doc: |
  ZIP is a popular archive file format that supports lossless data compression.
  A ZIP file may contain one or more files or directories that may have been compressed.
  The ZIP file format permits a number of compression algorithms.

seq:
  - id: sections
    type: section
    repeat: eos

types:
  section:
    seq:
      - id: header
        type: u4
      - id: body
        type:
          switch-on: header
          cases:
            '0x04034b50': local_file_header
            '0x02014b50': central_directory_file_header
            '0x06054b50': end_of_central_directory_record

  local_file_header:
    seq:
      - id: version_needed
        type: u2
      - id: gp_bit_flag
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
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length

  central_directory_file_header:
    seq:
      - id: version_made_by
        type: u2
      - id: version_needed
        type: u2
      - id: gp_bit_flag
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
      - id: local_header_offset
        type: u4
      - id: file_name
        type: str
        size: file_name_length
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
      - id: file_comment
        type: str
        size: file_comment_length
        encoding: UTF-8

  end_of_central_directory_record:
    seq:
      - id: disk_num
        type: u2
      - id: cd_start_disk
        type: u2
      - id: num_cd_records_on_disk
        type: u2
      - id: total_cd_records
        type: u2
      - id: cd_size
        type: u4
      - id: cd_offset
        type: u4
      - id: zip_comment_len
        type: u2
      - id: zip_comment
        type: str
        size: zip_comment_len
        encoding: UTF-8