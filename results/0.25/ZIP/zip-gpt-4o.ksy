meta:
  id: zip
  title: ZIP Archive
  file-extension: zip
  endian: le

seq:
  - id: sections
    type: section
    repeat: until
    repeat-until: _.signature == 0x06054b50

types:
  section:
    seq:
      - id: signature
        type: u4
      - id: body
        type:
          switch-on: signature
          cases:
            0x04034b50: local_file_header
            0x02014b50: central_directory_file_header
            0x06054b50: end_of_central_directory_record
            0x06064b50: zip64_end_of_central_directory_record
            0x07064b50: zip64_end_of_central_directory_locator
            0x08064b50: archive_extra_data_record
            0x05054b50: digital_signature
        size-eos: true

  local_file_header:
    seq:
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
        size: file_name_length
      - id: extra_field
        size: extra_field_length

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
      - id: relative_offset_of_local_header
        type: u4
      - id: file_name
        size: file_name_length
      - id: extra_field
        size: extra_field_length
      - id: file_comment
        size: file_comment_length

  end_of_central_directory_record:
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
        size: zip_file_comment_length

  zip64_end_of_central_directory_record:
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
      - id: extensible_data_sector
        size-eos: true

  zip64_end_of_central_directory_locator:
    seq:
      - id: number_of_the_disk_with_start_of_zip64_end_of_central_directory
        type: u4
      - id: relative_offset_of_zip64_end_of_central_directory_record
        type: u8
      - id: total_number_of_disks
        type: u4

  archive_extra_data_record:
    seq:
      - id: length_of_data
        type: u4
      - id: extra_field_data
        size: length_of_data

  digital_signature:
    seq:
      - id: size_of_data
        type: u2
      - id: signature_data
        size: size_of_data