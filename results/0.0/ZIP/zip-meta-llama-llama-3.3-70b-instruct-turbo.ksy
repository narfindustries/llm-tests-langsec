meta:
  id: zip
  endian: le

types:
  local_file_header:
    seq:
      - id: signature
        size: 4
      - id: version_needed_to_extract
        size: 2
      - id: general_purpose_bit_flag
        size: 2
      - id: compression_method
        size: 2
      - id: last_modification_time
        size: 2
      - id: last_modification_date
        size: 2
      - id: crc_32
        size: 4
      - id: compressed_size
        size: 4
      - id: uncompressed_size
        size: 4
      - id: filename_length
        size: 2
      - id: extra_field_length
        size: 2
      - id: filename
        size: filename_length
        type: str
        encoding: utf-8
      - id: extra_field
        size: extra_field_length
        type: bytes

  data_descriptor:
    seq:
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
      - id: version_needed_to_extract
        size: 2
      - id: general_purpose_bit_flag
        size: 2
      - id: compression_method
        size: 2
      - id: last_modification_time
        size: 2
      - id: last_modification_date
        size: 2
      - id: crc_32
        size: 4
      - id: compressed_size
        size: 4
      - id: uncompressed_size
        size: 4
      - id: filename_length
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
      - id: filename
        size: filename_length
        type: str
        encoding: utf-8
      - id: extra_field
        size: extra_field_length
        type: bytes
      - id: file_comment
        size: file_comment_length
        type: str
        encoding: utf-8

  end_of_central_directory:
    seq:
      - id: signature
        size: 4
      - id: number_of_this_disk
        size: 2
      - id: number_of_the_disk_where_the_central_directory_starts
        size: 2
      - id: number_of_entries_in_the_central_directory_on_this_disk
        size: 2
      - id: number_of_entries_in_the_central_directory
        size: 2
      - id: size_of_the_central_directory
        size: 4
      - id: offset_of_the_start_of_the_central_directory
        size: 4
      - id: comment_length
        size: 2
      - id: comment
        size: comment_length
        type: str
        encoding: utf-8

seq:
  - id: local_file_headers
    type: local_file_header
    repeat: expr
    repeat-expr: _root.local_file_headers.signature == 0x04034b50
  - id: file_data
    size: local_file_headers.compressed_size
    type: bytes
  - id: data_descriptor
    type: data_descriptor
    if: local_file_headers.general_purpose_bit_flag & 8 != 0
  - id: central_directory
    type: central_directory
    repeat: expr
    repeat-expr: _root.central_directory.signature == 0x02014b50
  - id: end_of_central_directory
    type: end_of_central_directory