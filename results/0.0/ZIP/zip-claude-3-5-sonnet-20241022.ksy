meta:
  id: zip
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
            0x04034b50: local_file
            0x02014b50: central_dir_entry
            0x06054b50: end_of_central_dir

  local_file:
    seq:
      - id: version_needed
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
        enum: compression
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
      - id: file_name_len
        type: u2
      - id: extra_field_len
        type: u2
      - id: file_name
        type: str
        size: file_name_len
        encoding: UTF-8
      - id: extra_field
        size: extra_field_len
      - id: body
        size: compressed_size

  central_dir_entry:
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
      - id: file_name_len
        type: u2
      - id: extra_field_len
        type: u2
      - id: file_comment_len
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
      - id: extra_field
        size: extra_field_len
      - id: file_comment
        type: str
        size: file_comment_len
        encoding: UTF-8

  end_of_central_dir:
    seq:
      - id: disk_number
        type: u2
      - id: disk_cd_start
        type: u2
      - id: num_entries_disk
        type: u2
      - id: num_entries_total
        type: u2
      - id: cd_size
        type: u4
      - id: cd_offset
        type: u4
      - id: comment_len
        type: u2
      - id: comment
        type: str
        size: comment_len
        encoding: UTF-8

  extra_field:
    seq:
      - id: header_id
        type: u2
      - id: data_size
        type: u2
      - id: data
        size: data_size

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
    18: ibm_terse
    19: ibm_lz77
    
  flags:
    0x0001: encrypted
    0x0002: compression_option1
    0x0004: compression_option2
    0x0008: data_descriptor
    0x0010: enhanced_deflation
    0x0020: compressed_patched
    0x0040: strong_encryption
    0x0800: utf8
    0x2000: mask_header_values