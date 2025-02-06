meta:
  id: zip
  file-extension: zip
  endian: le
  title: ZIP Archive
doc: |
  ZIP is a popular archive file format, used to compress and bundle multiple files together.

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
      - id: descriptor
        type: data_descriptor
        if: header.general_purpose_bit_flag & 0x08 != 0

  local_file_header:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x03, 0x04]
      - id: version_needed_to_extract
        type: u2
      - id: general_purpose_bit_flag
        type: u2
      - id: compression_method
        type: u2
      - id: last_mod_file_time
        type: dos_time
      - id: last_mod_file_date
        type: dos_date
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
        encoding: CP437
      - id: extra_field
        size: extra_field_length

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
        contents: [0x50, 0x4b, 0x01, 0x02]
      - id: version_made_by
        type: u2
      - id: version_needed_to_extract
        type: u2
      - id: general_purpose_bit_flag
        type: u2
      - id: compression_method
        type: u2
      - id: last_mod_file_time
        type: dos_time
      - id: last_mod_file_date
        type: dos_date
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
      - id: relative_offset_of_local_header
        type: u4
      - id: file_name
        type: str
        size: file_name_length
        encoding: CP437
      - id: extra_field
        size: extra_field_length
      - id: file_comment
        type: str
        size: file_comment_length
        encoding: CP437

  end_of_central_directory_record:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x05, 0x06]
      - id: number_of_this_disk
        type: u2
      - id: disk_where_central_directory_starts
        type: u2
      - id: number_of_central_directory_records_on_this_disk
        type: u2
      - id: total_number_of_central_directory_records
        type: u2
      - id: size_of_central_directory
        type: u4
      - id: offset_of_start_of_central_directory_relative_to_start_of_archive
        type: u4
      - id: zip_file_comment_length
        type: u2
      - id: zip_file_comment
        type: str
        size: zip_file_comment_length
        encoding: CP437

  dos_time:
    seq:
      - id: hour
        type: b5
      - id: minute
        type: b6
      - id: second
        type: b5

  dos_date:
    seq:
      - id: year
        type: b7
      - id: month
        type: b4
      - id: day
        type: b5