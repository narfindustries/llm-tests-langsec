meta:
  id: zip
  file-extension: zip
  endian: le
seq:
  - id: local_files
    type: local_file
    repeat: eos
types:
  local_file:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x03, 0x04]
      - id: version_needed
        type: u2
      - id: general_purpose
        type: u2
      - id: compression
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
      - id: file_name_length
        type: u2
      - id: extra_field_length
        type: u2
      - id: file_name
        size: file_name_length
        type: str
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
        type: extra_field
  extra_field:
    seq:
      - id: header_id
        type: u2
      - id: data_size
        type: u2
      - id: data
        size: data_size
  central_directory:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x01, 0x02]
      - id: version_made_by
        type: u2
      - id: version_needed
        type: u2
      - id: general_purpose
        type: u2
      - id: compression
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
      - id: file_name_length
        type: u2
      - id: extra_field_length
        type: u2
      - id: file_comment_length
        type: u2
      - id: disk_number_start
        type: u2
      - id: internal_attributes
        type: u2
      - id: external_attributes
        type: u4
      - id: relative_offset
        type: u4
      - id: file_name
        size: file_name_length
        type: str
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
        type: extra_field
      - id: file_comment
        size: file_comment_length
        type: str
        encoding: UTF-8
  end_of_central_directory:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x05, 0x06]
      - id: disk_number
        type: u2
      - id: start_disk
        type: u2
      - id: entries_on_disk
        type: u2
      - id: total_entries
        type: u2
      - id: size_of_cd
        type: u4
      - id: offset_of_cd
        type: u4
      - id: comment_length
        type: u2
      - id: comment
        size: comment_length
        type: str
        encoding: UTF-8
  zip64_end_of_central_directory:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x06, 0x06]
      - id: size_of_record
        type: u8
      - id: version_made_by
        type: u2
      - id: version_needed
        type: u2
      - id: disk_number
        type: u4
      - id: start_disk
        type: u4
      - id: entries_on_disk
        type: u8
      - id: total_entries
        type: u8
      - id: size_of_cd
        type: u8
      - id: offset_of_cd
        type: u8
  zip64_end_of_central_directory_locator:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x06, 0x07]
      - id: disk_number
        type: u4
      - id: offset_of_record
        type: u8
      - id: total_disks
        type: u4
  data_descriptor:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x07, 0x08]
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4