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
            0x0403: central_dir_entry
            0x0201: local_file
            0x0605: end_of_central_dir
            0x0606: zip64_end_of_central_dir
            0x0706: zip64_end_of_central_dir_locator

  local_file:
    seq:
      - id: version
        type: u2
      - id: flags
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
      - id: extra_len
        type: u2
      - id: file_name
        type: str
        size: file_name_len
        encoding: UTF-8
      - id: extra
        size: extra_len
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
        size: extra_len
      - id: comment
        type: str
        size: comment_len
        encoding: UTF-8

  end_of_central_dir:
    seq:
      - id: disk_number
        type: u2
      - id: disk_start
        type: u2
      - id: qty_central_dir_entries_on_disk
        type: u2
      - id: qty_central_dir_entries_total
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

  zip64_end_of_central_dir:
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
      - id: qty_central_dir_entries_on_disk
        type: u8
      - id: qty_central_dir_entries_total
        type: u8
      - id: central_dir_size
        type: u8
      - id: central_dir_offset
        type: u8
      - id: extensible_data
        size: record_size - 44

  zip64_end_of_central_dir_locator:
    seq:
      - id: disk_number_with_zip64_end_of_central_dir
        type: u4
      - id: zip64_end_of_central_dir_offset
        type: u8
      - id: number_of_disks
        type: u4