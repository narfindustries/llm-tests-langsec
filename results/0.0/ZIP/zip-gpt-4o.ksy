meta:
  id: zip
  title: ZIP Archive
  file-extension: zip
  xref:
    justsolve: ZIP
    mime: application/zip
  license: CC0-1.0
  endian: le

seq:
  - id: sections
    type: pk_section
    repeat: eos

types:
  pk_section:
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

  local_file_header:
    seq:
      - id: version
        type: u2
      - id: general_purpose_flags
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
      - id: file_name_len
        type: u2
      - id: extra_len
        type: u2
      - id: file_name
        size: file_name_len
        type: str
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
      - id: general_purpose_flags
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
      - id: file_name_len
        type: u2
      - id: extra_len
        type: u2
      - id: comment_len
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
      - id: extra
        size: extra_len
      - id: comment
        size: comment_len
        type: str
        encoding: UTF-8

  end_of_central_dir:
    seq:
      - id: disk_number
        type: u2
      - id: central_dir_disk
        type: u2
      - id: num_central_dir_entries_disk
        type: u2
      - id: num_central_dir_entries_total
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