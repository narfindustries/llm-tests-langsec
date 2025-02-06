meta:
  id: zip
  file-extension: zip
  endian: le

seq:
  - id: sections
    type: pk_section
    repeat: until
    repeat-until: _io.eof

types:
  pk_section:
    seq:
      - id: magic
        type: u4
        enum: magic_type
      - id: body
        type:
          switch-on: magic
          cases:
            'magic_type::local_file': local_file
            'magic_type::central_dir': central_dir
            'magic_type::end_of_central_dir': end_of_central_dir
            'magic_type::zip64_end_of_central_dir': zip64_end_of_central_dir
            'magic_type::zip64_end_of_central_dir_locator': zip64_end_of_central_dir_locator

  local_file:
    seq:
      - id: version_needed_to_extract
        type: u2
      - id: flags
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
      - id: file_name_length
        type: u2
      - id: extra_field_length
        type: u2
      - id: file_name
        type: str
        size: file_name_length
        encoding: UTF-8
      - id: extra_field
        type: extra_field
        size: extra_field_length
      - id: body
        size: compressed_size
      - id: data_descriptor
        type: data_descriptor
        if: flags & 0x8 != 0

  central_dir:
    seq:
      - id: version_made_by
        type: u2
      - id: version_needed_to_extract
        type: u2
      - id: flags
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
      - id: local_header_offset
        type: u4
      - id: file_name
        type: str
        size: file_name_length
        encoding: UTF-8
      - id: extra_field
        type: extra_field
        size: extra_field_length
      - id: file_comment
        type: str
        size: file_comment_length
        encoding: UTF-8

  end_of_central_dir:
    seq:
      - id: disk_number
        type: u2
      - id: disk_start
        type: u2
      - id: num_entries_this_disk
        type: u2
      - id: num_entries
        type: u2
      - id: central_dir_size
        type: u4
      - id: central_dir_offset
        type: u4
      - id: comment_length
        type: u2
      - id: comment
        type: str
        size: comment_length
        encoding: UTF-8

  zip64_end_of_central_dir:
    seq:
      - id: record_size
        type: u8
      - id: version_made_by
        type: u2
      - id: version_needed_to_extract
        type: u2
      - id: disk_number
        type: u4
      - id: disk_start_central_dir
        type: u4
      - id: num_entries_this_disk
        type: u8
      - id: num_entries
        type: u8
      - id: central_dir_size
        type: u8
      - id: central_dir_offset
        type: u8
      - id: zip64_extensible_data
        size: record_size - 44

  zip64_end_of_central_dir_locator:
    seq:
      - id: disk_number_with_zip64_end
        type: u4
      - id: end_of_central_dir_offset
        type: u8
      - id: total_number_of_disks
        type: u4

  data_descriptor:
    seq:
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4

  extra_field:
    seq:
      - id: entries
        type: extra_field_entry
        repeat: eos

  extra_field_entry:
    seq:
      - id: header_id
        type: u2
      - id: data_size
        type: u2
      - id: data
        size: data_size

enums:
  magic_type:
    0x04034b50: local_file
    0x02014b50: central_dir
    0x06054b50: end_of_central_dir
    0x06064b50: zip64_end_of_central_dir
    0x07064b50: zip64_end_of_central_dir_locator

  compression_method:
    0: none
    1: shrunk
    2: reduced_1
    3: reduced_2
    4: reduced_3
    5: reduced_4
    6: imploded
    8: deflated
    9: enhanced_deflated
    10: pkware_dcl_imploded
    12: bzip2
    14: lzma
    18: ibm_terse
    19: ibm_lz77
    97: wavpack
    98: ppmd