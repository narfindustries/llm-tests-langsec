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
      - id: section_type
        type: u4
        enum: section_signatures
      - id: content
        type:
          switch-on: section_type
          cases:
            'section_signatures::local_file_header': local_file_header
            'section_signatures::central_directory_file_header': central_directory_file_header
            'section_signatures::end_of_central_directory': end_of_central_directory
            'section_signatures::zip64_end_of_central_directory': zip64_end_of_central_directory

  local_file_header:
    seq:
      - id: version_needed_to_extract
        type: u2
      - id: general_purpose_bit_flag
        type: u2
        enum: general_purpose_flags
      - id: compression_method
        type: u2
        enum: compression_methods
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
        type: extra_field
        size: extra_field_length
      - id: compressed_data
        type: compressed_data
        size: compressed_size

  central_directory_file_header:
    seq:
      - id: version_made_by
        type: u2
      - id: version_needed_to_extract
        type: u2
      - id: general_purpose_bit_flag
        type: u2
        enum: general_purpose_flags
      - id: compression_method
        type: u2
        enum: compression_methods
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
        type: extra_field
        size: extra_field_length
      - id: file_comment
        type: str
        size: file_comment_length
        encoding: UTF-8

  end_of_central_directory:
    seq:
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
        encoding: UTF-8

  zip64_end_of_central_directory:
    seq:
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

  extra_field:
    seq:
      - id: extra_field_data
        type: u1
        repeat: eos

  compressed_data:
    seq:
      - id: data
        type: u1
        repeat: eos

enums:
  section_signatures:
    0x04034B50: local_file_header
    0x02014B50: central_directory_file_header
    0x06054B50: end_of_central_directory
    0x06064B50: zip64_end_of_central_directory

  general_purpose_flags:
    0b0000000000000001: encrypted
    0b0000000000000010: compression_option_1
    0b0000000000000100: compression_option_2
    0b0000000000001000: data_descriptor_present
    0b0000000000010000: enhanced_deflation
    0b0000000000100000: compressed_patched_data

  compression_methods:
    0: no_compression
    1: shrunk
    2: reduced_factor1
    3: reduced_factor2
    4: reduced_factor3
    5: reduced_factor4
    6: imploded
    8: deflated
    9: enhanced_deflated
    10: pkware_dcl_imploded
    12: bzip2
    14: lzma
    18: ibm_terse
    19: ibm_lz77_z
    98: blowfish
    99: twofish