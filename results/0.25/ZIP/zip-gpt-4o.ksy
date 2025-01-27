meta:
  id: zip
  title: ZIP Archive
  file-extension: zip
  xref:
    rfc: https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
  license: CC0-1.0
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
        size-eos: true
        type:
          switch-on: signature
          cases:
            0x04034b50: local_file_header
            0x02014b50: central_dir_entry
            0x06054b50: end_of_central_dir
            0x06064b50: zip64_end_of_central_dir
            0x07064b50: zip64_end_of_central_dir_locator
            0x08074b50: data_descriptor

  local_file_header:
    seq:
      - id: version
        type: u2
      - id: general_purpose_flags
        type: u2
      - id: compression_method
        type: u2
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
      - id: extra_field_len
        type: u2
      - id: file_name
        size: file_name_len
        type: str
        encoding: UTF-8
      - id: extra_field
        size: extra_field_len

  central_dir_entry:
    seq:
      - id: version_made_by
        type: u2
      - id: version_needed
        type: u2
      - id: general_purpose_flags
        type: u2
      - id: compression_method
        type: u2
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
      - id: extra_field_len
        type: u2
      - id: file_comment_len
        type: u2
      - id: disk_number_start
        type: u2
      - id: internal_file_attrs
        type: u2
      - id: external_file_attrs
        type: u4
      - id: local_header_offset
        type: u4
      - id: file_name
        size: file_name_len
        type: str
        encoding: UTF-8
      - id: extra_field
        size: extra_field_len
      - id: file_comment
        size: file_comment_len
        type: str
        encoding: UTF-8

  end_of_central_dir:
    seq:
      - id: disk_number
        type: u2
      - id: central_dir_disk
        type: u2
      - id: num_central_dir_records_on_disk
        type: u2
      - id: total_central_dir_records
        type: u2
      - id: central_dir_size
        type: u4
      - id: central_dir_offset
        type: u4
      - id: comment_len
        type: u2
      - id: comment
        size: comment_len
        type: str
        encoding: UTF-8

  zip64_end_of_central_dir:
    seq:
      - id: size_of_zip64_end_of_central_dir
        type: u8
      - id: version_made_by
        type: u2
      - id: version_needed
        type: u2
      - id: disk_number
        type: u4
      - id: central_dir_disk
        type: u4
      - id: num_central_dir_records_on_disk
        type: u8
      - id: total_central_dir_records
        type: u8
      - id: central_dir_size
        type: u8
      - id: central_dir_offset
        type: u8
      - id: extensible_data_sector
        size-eos: true

  zip64_end_of_central_dir_locator:
    seq:
      - id: central_dir_disk
        type: u4
      - id: zip64_end_of_central_dir_offset
        type: u8
      - id: num_disks
        type: u4

  data_descriptor:
    seq:
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4