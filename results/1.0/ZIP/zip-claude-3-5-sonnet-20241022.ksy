meta:
  id: zip
  file-extension: zip
  endian: le

seq:
  - id: sections
    type: pk_section
    repeat: eos

types:
  pk_section:
    seq:
      - id: magic
        contents: [0x50, 0x4B]
      - id: section_type
        type: u2
      - id: body
        type:
          switch-on: section_type
          cases:
            0x0403: pk_section_local_file
            0x0201: pk_section_central_dir
            0x0605: pk_section_end_of_central_dir
            0x0606: pk_section_zip64_end_of_central_dir
            0x0706: pk_section_zip64_end_of_central_dir_locator

  pk_section_local_file:
    seq:
      - id: version_needed
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
        enum: compression
      - id: file_mod_time
        type: u2
      - id: file_mod_date
        type: u2
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
      - id: file_name_len
        type: u2
      - id: extra_len
        type: u2
      - id: file_name
        type: str
        size: file_name_len
        encoding: UTF-8
      - id: extra
        type: extra_field
        size: extra_len
      - id: body
        size: compressed_size

  pk_section_central_dir:
    seq:
      - id: version_made_by
        type: u2
      - id: version_needed
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
        enum: compression
      - id: file_mod_time
        type: u2
      - id: file_mod_date
        type: u2
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
      - id: file_name_len
        type: u2
      - id: extra_len
        type: u2
      - id: comment_len
        type: u2
      - id: disk_number_start
        type: u2
      - id: internal_attrs
        type: u2
      - id: external_attrs
        type: u4
      - id: local_header_offset
        type: u4
      - id: file_name
        type: str
        size: file_name_len
        encoding: UTF-8
      - id: extra
        type: extra_field
        size: extra_len
      - id: comment
        type: str
        size: comment_len
        encoding: UTF-8

  pk_section_end_of_central_dir:
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
      - id: comment_len
        type: u2
      - id: comment
        type: str
        size: comment_len
        encoding: UTF-8

  pk_section_zip64_end_of_central_dir:
    seq:
      - id: record_size
        type: u8
      - id: version_made_by
        type: u2
      - id: version_needed
        type: u2
      - id: disk_number
        type: u4
      - id: disk_start
        type: u4
      - id: num_entries_this_disk
        type: u8
      - id: num_entries
        type: u8
      - id: central_dir_size
        type: u8
      - id: central_dir_offset
        type: u8
      - id: extensible_data
        size: record_size - 44

  pk_section_zip64_end_of_central_dir_locator:
    seq:
      - id: disk_number_with_zip64_end_cd
        type: u4
      - id: zip64_end_of_central_dir_offset
        type: u8
      - id: total_number_of_disks
        type: u4

  extra_field:
    seq:
      - id: entries
        type: extra_field_entry
        repeat: eos

  extra_field_entry:
    seq:
      - id: code
        type: u2
      - id: size
        type: u2
      - id: body
        size: size

enums:
  compression:
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
    98: ppmd