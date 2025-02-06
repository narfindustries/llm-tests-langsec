The error "mapping values are not allowed here" in Kaitai Struct usually means you've used a dictionary (YAML mapping) where a sequence is expected, or vice-versa.  The previous attempts likely had an issue with the `repeat: eos`  within the `zip_file` type.  `eos` is a valid option for `repeat`, but it needs to be used correctly within a sequence.

The following attempts to correct this by restructuring the `zip_file` type to ensure that the `repeat: eos` is used appropriately within a sequence.  It also addresses potential issues with the `enum` definitions by ensuring they are correctly formatted.


types:
  zip_end_central_directory_record:
    seq:
      - id: signature
        type: u4
        enum:
          0x06054b50: zip64_end_central_directory_locator
          0x02014b53: end_central_directory_record
      - id: disk_number
        type: u2le
      - id: disk_with_central_directory
        type: u2le
      - id: central_directory_entries_on_this_disk
        type: u4le
      - id: central_directory_entries_total
        type: u4le
      - id: central_directory_size
        type: u4le
      - id: central_directory_offset
        type: u4le
      - id: zip_comment_length
        type: u2le
      - id: zip_comment
        type: str
        size: zip_comment_length

  zip64_end_central_directory_locator:
    seq:
      - id: signature
        type: u4
        enum: 0x08074b50: zip64_end_central_directory_locator
      - id: disk_with_zip64_end_of_central_directory
        type: u4le
      - id: relative_offset_to_zip64_end_of_central_directory
        type: u8le
      - id: total_number_of_disks
        type: u4le

  zip64_end_central_directory_record:
    seq:
      - id: signature
        type: u4
        enum: 0x06064b50: zip64_end_central_directory_record
      - id: size_of_zip64_end_of_central_directory_record
        type: u8le
      - id: version_made_by
        type: u2le
      - id: version_needed_to_extract
        type: u2le
      - id: number_of_this_disk
        type: u4le
      - id: number_of_the_disk_with_the_start_of_the_central_directory
        type: u4le
      - id: total_number_of_entries_in_the_central_directory_on_this_disk
        type: u8le
      - id: total_number_of_entries_in_the_central_directory
        type: u8le
      - id: size_of_the_central_directory
        type: u8le
      - id: offset_of_the_central_directory
        type: u8le
      - id: zip64_extensible_data_sector
        type: bytes
        size: 0


  local_file_header:
    seq:
      - id: signature
        type: u4
        enum: 0x04034b50: local_file_header
      - id: version_needed_to_extract
        type: u2le
      - id: general_purpose_bit_flag
        type: u2le
      - id: compression_method
        type: u2le
      - id: last_mod_time
        type: u2le
      - id: last_mod_date
        type: u2le
      - id: crc32
        type: u4le
      - id: compressed_size
        type: u4le
      - id: uncompressed_size
        type: u4le
      - id: filename_length
        type: u2le
      - id: extra_field_length
        type: u2le
      - id: filename
        type: str
        size: filename_length
      - id: extra_field
        type: bytes
        size: extra_field_length
      - id: compressed_data
        type: bytes
        size: compressed_size


  central_directory_header:
    seq:
      - id: signature
        type: u4
        enum: 0x02014b50: central_directory_header
      - id: version_made_by
        type: u2le
      - id: version_needed_to_extract
        type: u2le
      - id: general_purpose_bit_flag
        type: u2le
      - id: compression_method
        type: u2le
      - id: last_mod_time
        type: u2le
      - id: last_mod_date
        type: u2le
      - id: crc32
        type: u4le
      - id: compressed_size
        type: u4le
      - id: uncompressed_size
        type: u4le
      - id: filename_length
        type: u2le
      - id: extra_field_length
        type: u2le
      - id: file_comment_length
        type: u2le
      - id: disk_number_start
        type: u2le
      - id: internal_file_attributes
        type: u2le
      - id: external_file_attributes
        type: u4le
      - id: local_header_offset
        type: u4le
      - id: filename
        type: str
        size: filename_length
      - id: extra_field
        type: bytes
        size: extra_field_length
      - id: file_comment
        type: str
        size: file_comment_length

  zip_file:
    seq:
      - id: central_directory_entries
        type: central_directory_header
        repeat: eos
      - id: end_central_directory
        type: zip_end_central_directory_record

