meta:
  id: zip
  file-extension: zip
  endian: le
seq:
  - id: signature
    type: u4
    valid: 0x04034b50
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
    type: str
    encoding: UTF-8
    size: file_name_length
  - id: extra_field
    type: u1
    repeat: expr
    repeat-expr: extra_field_length
  - id: data
    type: u1
    repeat: expr
    repeat-expr: compressed_size
types:
  local_file_header:
    seq:
      - id: signature
        type: u4
        valid: 0x04034b50
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
        type: str
        encoding: UTF-8
        size: file_name_length
      - id: extra_field
        type: u1
        repeat: expr
        repeat-expr: extra_field_length
      - id: data
        type: u1
        repeat: expr
        repeat-expr: compressed_size
  central_directory_file_header:
    seq:
      - id: signature
        type: u4
        valid: 0x02014b50
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
      - id: internal_file_attributes
        type: u2
      - id: external_file_attributes
        type: u4
      - id: relative_offset_of_local_header
        type: u4
      - id: file_name
        type: str
        encoding: UTF-8
        size: file_name_length
      - id: extra_field
        type: u1
        repeat: expr
        repeat-expr: extra_field_length
      - id: file_comment
        type: str
        encoding: UTF-8
        size: file_comment_length
  end_of_central_directory_record:
    seq:
      - id: signature
        type: u4
        valid: 0x06054b50
      - id: disk_number
        type: u2
      - id: disk_number_with_cd
        type: u2
      - id: num_entries_on_disk
        type: u2
      - id: num_entries_total
        type: u2
      - id: size_of_cd
        type: u4
      - id: offset_of_cd
        type: u4
      - id: comment_length
        type: u2
      - id: comment
        type: str
        encoding: UTF-8
        size: comment_length
instances:
  local_file_headers:
    type: local_file_header
    repeat: eos
  central_directory_file_headers:
    type: central_directory_file_header
    repeat: eos
  end_of_central_directory_record:
    type: end_of_central_directory_record
    repeat: eos