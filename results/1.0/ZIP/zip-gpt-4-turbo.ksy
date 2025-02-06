meta:
  id: zip_file
  title: ZIP Archive
  file-extension: zip
  endian: le
  license: CC0-1.0
doc: |
  ZIP is a popular archive file format that supports lossless data compression.
  A ZIP file may contain one or more files or directories that may have been
  compressed. The ZIP file format permits a number of compression algorithms.

seq:
  - id: sections
    type: section
    repeat: eos

types:
  section:
    seq:
      - id: header
        type: local_file_header
      - id: body
        size: header.compressed_size
      - id: central_dir
        type: central_directory_entry

  local_file_header:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x03, 0x04]
      - id: version_needed
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
      - id: last_mod_time
        type: dos_time
      - id: last_mod_date
        type: dos_date
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

  central_directory_entry:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x01, 0x02]
      - id: version_made_by
        type: u2
      - id: version_needed
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
      - id: last_mod_time
        type: dos_time
      - id: last_mod_date
        type: dos_date
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
      - id: file_comment
        type: str
        size: comment_len
        encoding: UTF-8

  dos_time:
    seq:
      - id: hour
        type: b5
      - id: minute
        type: b6
      - id: second_div_2
        type: b5

  dos_date:
    seq:
      - id: year_minus_1980
        type: b7
      - id: month
        type: b4
      - id: day
        type: b5