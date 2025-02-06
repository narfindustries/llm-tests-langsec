types:
  zip_end_central_dir_record:
    seq:
      - id: signature
        type: u4
        enum:
          zip_end_central_dir_signature: 0x06054b50
      - id: num_this_disk
        type: u2
      - id: num_start_disk
        type: u2
      - id: central_dir_entries_on_this_disk
        type: u2
      - id: central_dir_entries_total
        type: u2
      - id: central_dir_size
        type: u4
      - id: central_dir_offset
        type: u4
      - id: zip_comment_length
        type: u2
      - id: zip_comment
        type: str
        size: zip_comment_length

  zip_central_dir_entry:
    seq:
      - id: signature
        type: u4
        enum:
          zip_central_dir_signature: 0x02014b50
      - id: version_made_by
        type: u2
      - id: version_needed_to_extract
        type: u2
      - id: general_purpose_bit_flag
        type: u2
      - id: compression_method
        type: u2
      - id: last_mod_time
        type: u2
      - id: last_mod_date
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
      - id: relative_offset_local_header
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
          zip_local_file_header_signature: 0x04034b50
      - id: version_needed_to_extract
        type: u2
      - id: general_purpose_bit_flag
        type: u2
      - id: compression_method
        type: u2
      - id: last_mod_time
        type: u2
      - id: last_mod_date
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

  zip_data_descriptor:
    seq:
      - id: signature
        type: u4
        enum:
          zip_data_descriptor_signature: 0x08074b50
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4

root:
  seq:
    - id: files
      type: zip_local_file_header
      repeat: eos
    - id: central_directory
      type: zip_central_dir_entry
      repeat: eos
    - id: end_of_central_directory_record
      type: zip_end_central_dir_record

