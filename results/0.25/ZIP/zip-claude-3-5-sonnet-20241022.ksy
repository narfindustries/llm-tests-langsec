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
      - id: local_file
        type: local_file_header
        if: signature == 0x04034b50
      - id: central_dir
        type: central_dir_header
        if: signature == 0x02014b50
      - id: end_of_central_dir
        type: end_of_central_dir
        if: signature == 0x06054b50

  local_file_header:
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
    instances:
      version_major:
        value: version_needed >> 8
      version_minor:
        value: version_needed & 0xFF
      is_encrypted:
        value: (flags & 0x1) != 0
      has_data_descriptor:
        value: (flags & 0x8) != 0
      has_utf8:
        value: (flags & 0x800) != 0
      is_compressed_patched:
        value: (flags & 0x20) != 0
      is_strong_encrypted:
        value: (flags & 0x40) != 0

  central_dir_header:
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
      - id: extra_field
        size: extra_field_len
      - id: comment
        type: str
        size: comment_len
        encoding: UTF-8
    instances:
      version_major:
        value: version_needed >> 8
      version_minor:
        value: version_needed & 0xFF
      is_encrypted:
        value: (flags & 0x1) != 0
      has_data_descriptor:
        value: (flags & 0x8) != 0
      has_utf8:
        value: (flags & 0x800) != 0
      is_compressed_patched:
        value: (flags & 0x20) != 0
      is_strong_encrypted:
        value: (flags & 0x40) != 0

  end_of_central_dir:
    seq:
      - id: disk_number
        type: u2
      - id: disk_start
        type: u2
      - id: num_entries_this_disk
        type: u2
      - id: num_entries_total
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
    96: jpeg_variant
    97: wavpack
    98: ppmd
    99: aes_encrypted