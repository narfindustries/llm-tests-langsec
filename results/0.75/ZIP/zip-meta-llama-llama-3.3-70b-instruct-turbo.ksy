meta:
  id: zip
  title: ZIP file format
  file-extension: zip
  endian: le
seq:
  - id: local_file_headers
    type: local_file_header
    repeat: until false
  - id: central_directory_headers
    type: central_directory_header
    repeat: until false
  - id: end_of_central_directory
    type: end_of_central_directory_record
types:
  local_file_header:
    seq:
      - id: signature
        type: uint4le
        contents: 0x04034b50
      - id: version_needed_to_extract
        type: uint2le
      - id: general_purpose_bit_flag
        type: uint2le
      - id: compression_method
        type: uint2le
      - id: last_modified_time
        type: uint2le
      - id: last_modified_date
        type: uint2le
      - id: crc_32
        type: uint4le
      - id: compressed_size
        type: uint4le
      - id: uncompressed_size
        type: uint4le
      - id: file_name_length
        type: uint2le
      - id: extra_field_length
        type: uint2le
      - id: file_name
        size: file_name_length
        type: str
        encoding: utf-8
      - id: extra_field
        size: extra_field_length
        type: extra_field
      - id: file_data
        size: compressed_size
        type: bytes
      - id: data_descriptor
        type: data_descriptor
        if: general_purpose_bit_flag & 8 != 0
  extra_field:
    seq:
      - id: header_id
        type: uint2le
      - id: data_size
        type: uint2le
      - id: data
        size: data_size
        type: bytes
  data_descriptor:
    seq:
      - id: signature
        type: uint4le
        contents: 0x08074b50
      - id: crc_32
        type: uint4le
      - id: compressed_size
        type: uint4le
      - id: uncompressed_size
        type: uint4le
  central_directory_header:
    seq:
      - id: signature
        type: uint4le
        contents: 0x02014b50
      - id: version_made_by
        type: uint2le
      - id: version_needed_to_extract
        type: uint2le
      - id: general_purpose_bit_flag
        type: uint2le
      - id: compression_method
        type: uint2le
      - id: last_modified_time
        type: uint2le
      - id: last_modified_date
        type: uint2le
      - id: crc_32
        type: uint4le
      - id: compressed_size
        type: uint4le
      - id: uncompressed_size
        type: uint4le
      - id: file_name_length
        type: uint2le
      - id: extra_field_length
        type: uint2le
      - id: file_comment_length
        type: uint2le
      - id: disk_number_start
        type: uint2le
      - id: internal_file_attributes
        type: uint2le
      - id: external_file_attributes
        type: uint4le
      - id: local_header_offset
        type: uint4le
      - id: file_name
        size: file_name_length
        type: str
        encoding: utf-8
      - id: extra_field
        size: extra_field_length
        type: extra_field
      - id: file_comment
        size: file_comment_length
        type: str
        encoding: utf-8
  end_of_central_directory_record:
    seq:
      - id: signature
        type: uint4le
        contents: 0x06054b50
      - id: number_of_this_disk
        type: uint2le
      - id: number_of_the_disk_where_the_central_directory_starts
        type: uint2le
      - id: number_of_entries_in_this_disk
        type: uint2le
      - id: total_number_of_entries
        type: uint2le
      - id: size_of_central_directory
        type: uint4le
      - id: offset_of_start_of_central_directory
        type: uint4le
      - id: comment_length
        type: uint2le
      - id: comment
        size: comment_length
        type: str
        encoding: utf-8