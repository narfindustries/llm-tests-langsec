meta:
  id: zip_file
  file-extension: zip
  endian: le

types:
  file_sections:
    seq:
      - id: entries
        type: local_file_header
        repeat: eos

  local_file_header:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x03, 0x04]
      - id: version_extract
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
      - id: last_mod_time
        type: u2
      - id: last_mod_date
        type: u2
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
      - id: filename_length
        type: u2
      - id: extra_field_length
        type: u2
      - id: filename
        type: str
        size: filename_length
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
      - id: compressed_data
        size: compressed_size

  central_directory_header:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x01, 0x02]
      - id: version_made_by
        type: u2
      - id: version_extract
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
      - id: last_mod_time
        type: u2
      - id: last_mod_date
        type: u2
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
      - id: filename_length
        type: u2
      - id: extra_field_length
        type: u2
      - id: file_comment_length
        type: u2
      - id: disk_number_start
        type: u2
      - id: internal_file_attrs
        type: u2
      - id: external_file_attrs
        type: u4
      - id: local_header_offset
        type: u4
      - id: filename
        type: str
        size: filename_length
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
      - id: file_comment
        type: str
        size: file_comment_length
        encoding: UTF-8

  end_of_central_directory:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x05, 0x06]
      - id: disk_number
        type: u2
      - id: disk_central_dir
        type: u2
      - id: records_on_disk
        type: u2
      - id: total_records
        type: u2
      - id: central_dir_size
        type: u4
      - id: central_dir_offset
        type: u4
      - id: comment_length
        type: u2
      - id: comment
        type: str
        size: comment_length
        encoding: UTF-8

  zip64_end_of_central_directory:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x06, 0x06]
      - id: record_size
        type: u8
      - id: version_made_by
        type: u2
      - id: version_extract
        type: u2
      - id: disk_number
        type: u4
      - id: disk_central_dir
        type: u4
      - id: records_on_disk
        type: u8
      - id: total_records
        type: u8
      - id: central_dir_size
        type: u8
      - id: central_dir_offset
        type: u8

seq:
  - id: sections
    type: file_sections