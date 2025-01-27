meta:
  id: zip
  file-extension: zip
  endian: le
seq:
  - id: local_file_header
    type: local_file_header
  - id: file_data
    size: local_file_header.compressed_size
  - id: data_descriptor
    type: data_descriptor
    if: local_file_header.flags & 0x08 != 0
  - id: central_directory
    type: central_directory
types:
  local_file_header:
    seq:
      - id: signature
        contents: [0x50, 0x4B, 0x03, 0x04]
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
  data_descriptor:
    seq:
      - id: signature
        contents: [0x50, 0x4B, 0x07, 0x08]
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
  central_directory:
    seq:
      - id: signature
        contents: [0x50, 0x4B, 0x01, 0x02]
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
      - id: file_name_length
        type: u2
      - id: extra_field_length
        type: u2
      - id: file_comment_length
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
        size: file_name_length
        type: str
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
      - id: file_comment
        size: file_comment_length
        type: str
        encoding: UTF-8
  end_of_central_directory:
    seq:
      - id: signature
        contents: [0x50, 0x4B, 0x05, 0x06]
      - id: disk_number
        type: u2
      - id: disk_number_start
        type: u2
      - id: num_entries_on_disk
        type: u2
      - id: total_entries
        type: u2
      - id: central_directory_size
        type: u4
      - id: central_directory_offset
        type: u4
      - id: comment_length
        type: u2
      - id: comment
        size: comment_length
        type: str
        encoding: UTF-8