meta:
  id: zip_file
  file-extension: zip
  endian: le

seq:
  - id: sections
    type: section
    repeat: eos

types:
  section:
    seq:
      - id: magic
        type: u4
        enum: section_type
      - id: body
        type:
          switch-on: magic
          cases:
            'section_type::local_file_header': local_file_header
            'section_type::central_directory_file_header': central_directory_file_header
            'section_type::end_of_central_directory': end_of_central_directory
            'section_type::zip64_end_of_central_directory': zip64_end_of_central_directory

  local_file_header:
    seq:
      - id: version_needed_to_extract
        type: u2
      - id: general_purpose_bit_flag
        type: u2
      - id: compression_method
        type: u2
        enum: compression_method
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

  central_directory_file_header:
    seq:
      - id: version_made_by
        type: u2
      - id: version_needed_to_extract
        type: u2
      - id: general_purpose_bit_flag
        type: u2
      - id: compression_method
        type: u2
        enum: compression_method
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
      - id: filename_length
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
      - id: number_of_this_disk
        type: u2
      - id: number_of_disk_with_start_of_central_directory
        type: u2
      - id: total_entries_on_this_disk
        type: u2
      - id: total_entries_in_central_directory
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
        encoding: UTF-8

  zip64_end_of_central_directory:
    seq:
      - id: size_of_record
        type: u8
      - id: version_made_by
        type: u2
      - id: version_needed_to_extract
        type: u2
      - id: number_of_this_disk
        type: u4
      - id: number_of_disk_with_start_of_central_directory
        type: u4
      - id: total_entries_on_this_disk
        type: u8
      - id: total_entries_in_central_directory
        type: u8
      - id: size_of_central_directory
        type: u8
      - id: offset_of_start_of_central_directory
        type: u8

enums:
  section_type:
    0x04034B50: local_file_header
    0x02014B50: central_directory_file_header
    0x06054B50: end_of_central_directory
    0x06064B50: zip64_end_of_central_directory

  compression_method:
    0: no_compression
    1: shrink
    2: reduce_factor1
    3: reduce_factor2
    4: reduce_factor3
    5: reduce_factor4
    6: implode
    8: deflate
    9: deflate64
    12: bzip2
    14: lzma
    18: ibm_terse
    19: lz77
    98: ppmd