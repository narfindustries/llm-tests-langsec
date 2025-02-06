meta:
  id: zip
  file-extension: zip
  endian: le
  title: ZIP file format

types:
  local_file_header:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x03, 0x04]
      - id: version_needed_to_extract
        size: 2
      - id: general_purpose_bit_flag
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
        type: str
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
        type: extra_field

  central_directory:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x01, 0x02]
      - id: version_made_by
        size: 2
      - id: version_needed_to_extract
        size: 2
      - id: general_purpose_bit_flag
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
      - id: internal_attributes
        size: 2
      - id: external_attributes
        size: 4
      - id: local_header_offset
        size: 4
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

  data_descriptor:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x07, 0x08]
      - id: crc_32
        size: 4
      - id: compressed_size
        size: 4
      - id: uncompressed_size
        size: 4

  extra_field:
    seq:
      - id: header_id
        size: 2
      - id: data_size
        size: 2
      - id: data
        size: data_size
        type: bytes

  end_of_central_directory:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x05, 0x06]
      - id: number_of_this_disk
        size: 2
      - id: number_of_the_disk_where_the_central_directory_starts
        size: 2
      - id: number_of_entries_in_this_disk
        size: 2
      - id: total_number_of_entries
        size: 2
      - id: size_of_central_directory
        size: 4
      - id: offset_of_start_of_central_directory
        size: 4
      - id: zipfile_comment_length
        size: 2
      - id: zipfile_comment
        size: zipfile_comment_length
        type: str
        encoding: UTF-8

seq:
  - id: local_file_headers
    type: local_file_header
    repeat: until local_file_headers.signature != [0x50, 0x4b, 0x03, 0x04]
  - id: file_data
    size: local_file_headers.compressed_size
    type: bytes
  - id: data_descriptor
    type: data_descriptor
    if: local_file_headers.general_purpose_bit_flag & 8 != 0
  - id: central_directory
    type: central_directory
    repeat: until central_directory.signature != [0x50, 0x4b, 0x01, 0x02]
  - id: end_of_central_directory
    type: end_of_central_directory