meta:
  id: zip
  endian: le
  file-extension: zip
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
      - id: last_mod_time
        size: 2
      - id: last_mod_date
        size: 2
      - id: crc32
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
        encoding: UTF-8
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
      - id: crc32
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
      - id: last_mod_time
        size: 2
      - id: last_mod_date
        size: 2
      - id: crc32
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
      - id: int_file_attributes
        size: 2
      - id: ext_file_attributes
        size: 4
      - id: local_header_offset
        size: 4
      - id: filename
        size: filename_length
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
        type: extra_field
      - id: file_comment
        size: file_comment_length
        encoding: UTF-8
  end_of_central_directory:
    seq:
      - id: signature
        size: 4
      - id: number_of_this_disk
        size: 2
      - id: number_of_the_disk_with_the_start_of_the_central_directory
        size: 2
      - id: total_number_of_entries_in_the_central_directory_on_this_disk
        size: 2
      - id: total_number_of_entries_in_the_central_directory
        size: 2
      - id: size_of_the_central_directory
        size: 4
      - id: offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number
        size: 4
      - id: zipfile_comment_length
        size: 2
      - id: zipfile_comment
        size: zipfile_comment_length
        encoding: UTF-8
seq:
  - id: local_file_headers
    type: local_file_header
    repeat: expr
    repeat-expr: _io.size() - _io.pos() >= local_file_header.sizeof()
    until: local_file_header.signature != 0x04034b50
  - id: file_data
    size: local_file_header.compressed_size
    process!:
      xz: {lz77, lzw}
  - id: data_descriptor
    type: data_descriptor
    if: local_file_header.general_purpose_bit_flag & 0x0008 != 0
  - id: central_directory
    type: central_directory
    repeat: expr
    repeat-expr: end_of_central_directory.total_number_of_entries_in_the_central_directory
  - id: end_of_central_directory
    type: end_of_central_directory