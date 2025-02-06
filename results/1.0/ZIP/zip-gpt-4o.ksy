meta:
  id: zip
  title: ZIP Archive
  application: PKWARE ZIP
  license: MIT
  endian: le

seq:
  - id: files
    type: file
    repeat: until
    repeat-until: _.signature != 0x04034b50

  - id: central_directory
    type: central_directory_file_header
    repeat: eos

types:
  file:
    seq:
      - id: signature
        type: u4
      - id: version_needed
        type: u2
      - id: flags
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
      - id: len_file_name
        type: u2
      - id: len_extra_field
        type: u2
      - id: file_name
        size: len_file_name
      - id: extra_field
        size: len_extra_field
      - id: body
        size: compressed_size
      - id: data_descriptor
        type: data_descriptor
        if: '((flags & 0x0008) != 0)'

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
      - id: signature
        type: u4
      - id: version_made_by
        type: u2
      - id: version_needed
        type: u2
      - id: flags
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
      - id: len_file_name
        type: u2
      - id: len_extra_field
        type: u2
      - id: len_file_comment
        type: u2
      - id: disk_number_start
        type: u2
      - id: internal_file_attr
        type: u2
      - id: external_file_attr
        type: u4
      - id: relative_offset
        type: u4
      - id: file_name
        size: len_file_name
      - id: extra_field
        size: len_extra_field
      - id: file_comment
        size: len_file_comment

  end_of_central_directory:
    seq:
      - id: signature
        type: u4
      - id: disk_num
        type: u2
      - id: central_dir_start_disk
        type: u2
      - id: num_records_on_disk
        type: u2
      - id: total_central_dir_records
        type: u2
      - id: central_dir_size
        type: u4
      - id: central_dir_offset
        type: u4
      - id: len_comment
        type: u2
      - id: comment
        size: len_comment

  zip64_end_of_central_directory:
    seq:
      - id: signature
        type: u4
      - id: size_of_zip64_eocd
        type: u8
      - id: version_made_by
        type: u2
      - id: version_needed
        type: u2
      - id: disk_num
        type: u4
      - id: central_dir_start_disk
        type: u4
      - id: num_records_on_disk
        type: u8
      - id: total_central_dir_records
        type: u8
      - id: central_dir_size
        type: u8
      - id: central_dir_offset
        type: u8
      - id: extensible_data_sector
        size: size_of_zip64_eocd - 44

enums:
  compression_method:
    0: none
    1: shrunk
    8: deflated
    9: enhanced_deflated