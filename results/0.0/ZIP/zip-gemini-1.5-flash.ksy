types:
  zip_end_central_directory_record:
    seq:
      - id: signature
        type: u4
        enum:
          0x06054b50: end_central_directory
      - id: disk_number
        type: u2
      - id: disk_with_central_directory
        type: u2
      - id: central_directory_entries_on_this_disk
        type: u2
      - id: central_directory_entries_total
        type: u2
      - id: central_directory_size
        type: u4
      - id: central_directory_offset
        type: u4
      - id: zip_comment_length
        type: u2
      - id: zip_comment
        type: str
        size: zip_comment_length
  zip_central_directory_entry:
    seq:
      - id: signature
        type: u4
        enum:
          0x02014b50: central_directory_entry
      - id: version_made_by
        type: u2
      - id: version_needed_to_extract
        type: u2
      - id: general_purpose_bit_flag
        type: u2
      - id: compression_method
        type: u2
      - id: last_modified_time
        type: u2
      - id: last_modified_date
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
      - id: local_header_offset
        type: u4
      - id: filename
        type: str
        size: filename_length
      - id: extra_field
        type: bytes
        size: extra_field_length
      - id: file_comment
        type: str
        size: file_comment_length
  zip_local_file_header:
    seq:
      - id: signature
        type: u4
        enum:
          0x04034b50: local_file_header
      - id: version_needed_to_extract
        type: u2
      - id: general_purpose_bit_flag
        type: u2
      - id: compression_method
        type: u2
      - id: last_modified_time
        type: u2
      - id: last_modified_date
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
      - id: extra_field
        type: bytes
        size: extra_field_length
      - id: compressed_data
        type: bytes
        size: compressed_size
  zip:
    seq:
      - id: central_directory
        type: zip_central_directory_entry
        repeat: eos
      - id: end_central_directory_record
        type: zip_end_central_directory_record

