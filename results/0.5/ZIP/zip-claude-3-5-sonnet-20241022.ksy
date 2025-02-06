meta:
  id: zip
  file-extension: zip
  endian: le

seq:
  - id: sections
    type: section
    repeat: until
    repeat-until: _io.eof
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
      - id: last_mod_time
        type: u2
      - id: last_mod_date
        type: u2
      - id: crc32
        type: u4
      - id: len_body
        type: u4
      - id: uncompressed_size
        type: u4
      - id: len_file_name
        type: u2
      - id: len_extra_field
        type: u2
      - id: file_name
        type: str
        size: len_file_name
        encoding: UTF-8
      - id: extra_field
        type: extra_field
        size: len_extra_field
      - id: body
        size: len_body
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
      - id: local_header_offset
        type: u4
      - id: file_name
        type: str
        size: len_file_name
        encoding: UTF-8
      - id: extra_field
        type: extra_field
        size: len_extra_field
      - id: file_comment
        type: str
        size: len_file_comment
        encoding: UTF-8
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
      - id: len_comment
        type: u2
      - id: comment
        type: str
        size: len_comment
        encoding: UTF-8
  extra_field:
    seq:
      - id: entries
        type: extra_field_entry
        repeat: eos
  extra_field_entry:
    seq:
      - id: header_id
        type: u2
      - id: len_data
        type: u2
      - id: data
        size: len_data
        type:
          switch-on: header_id
          cases:
            0x0001: zip64_extended_info
            0x000a: ntfs_extra
            0x5455: extended_timestamp
            0x7875: unix_extra
            _: raw_extra_field
  zip64_extended_info:
    seq:
      - id: original_size
        type: u8
      - id: compressed_size
        type: u8
      - id: local_header_offset
        type: u8
      - id: disk_start
        type: u4
  ntfs_extra:
    seq:
      - id: reserved
        type: u4
      - id: attributes
        type: ntfs_attribute
        repeat: eos
  ntfs_attribute:
    seq:
      - id: tag
        type: u2
      - id: len_data
        type: u2
      - id: data
        size: len_data
  extended_timestamp:
    seq:
      - id: flags
        type: u1
      - id: mod_time
        type: u4
        if: flags & 0x1 != 0
      - id: access_time
        type: u4
        if: flags & 0x2 != 0
      - id: create_time
        type: u4
        if: flags & 0x4 != 0
  unix_extra:
    seq:
      - id: version
        type: u1
      - id: len_uid
        type: u1
      - id: uid
        size: len_uid
      - id: len_gid
        type: u1
      - id: gid
        size: len_gid
  raw_extra_field:
    seq:
      - id: data
        size-eos: true
enums:
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
    19: lz77
    95: xz
    96: jpeg
    97: wavpack
    98: ppmd
    99: aes