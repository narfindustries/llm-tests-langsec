meta:
  id: zip
  title: ZIP Archive
  file-extension: zip
  xref:
    url: https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
  endian: le
seq:
  - id: sections
    type: section
    repeat: eos
types:
  section:
    seq:
      - id: signature
        type: u4
      - id: body
        type:
          switch-on: signature
          cases:
            '0x04034b50': local_file_header
            '0x02014b50': central_directory_file_header
            '0x06054b50': end_of_central_directory_record
            '0x06064b50': zip64_end_of_central_directory_record
            '0x07064b50': zip64_end_of_central_directory_locator
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
        type: str
        size: file_name_length
        encoding: ASCII
      - id: extra_field
        size: extra_field_length
      - id: file_data
        size: compressed_size
      - id: data_descriptor
        type: data_descriptor
        if: '(general_purpose_bit_flag & 0x0008) != 0'
  data_descriptor:
    seq:
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
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
        type: str
        size: file_name_length
        encoding: ASCII
      - id: extra_field
        size: extra_field_length
      - id: file_comment
        type: str
        size: file_comment_length
        encoding: ASCII
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
        type: str
        size: zip_file_comment_length
        encoding: ASCII
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
      - id: number_of_disk_with_start_of_central_directory
        type: u4
      - id: total_number_of_entries_in_central_directory_on_this_disk
        type: u8
      - id: total_number_of_entries_in_central_directory
        type: u8
      - id: size_of_central_directory
        type: u8
      - id: offset_of_start_of_central_directory_with_respect_to_starting_disk_number
        type: u8
      - id: extensible_data_sector
        size-eos: true
  zip64_end_of_central_directory_locator:
    seq:
      - id: number_of_disk_with_start_of_zip64_end_of_central_directory
        type: u4
      - id: relative_offset_of_zip64_end_of_central_directory_record
        type: u8
      - id: total_number_of_disks
        type: u4