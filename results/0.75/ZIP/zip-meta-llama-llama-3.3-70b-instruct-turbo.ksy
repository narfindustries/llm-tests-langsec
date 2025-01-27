meta:
  id: zip
  file-extension: zip
  title: Zip archive
  license: MIT
  encoding: UTF-8
  endian: le

seq:
  - id: local_file_header
    type: local_file_header
    Repeat: eos

types:
  local_file_header:
    seq:
      - id: signature
        size: 4
      - id: version_needed_to_extract
        size: 2
      - id: flags
        size: 2
      - id: compression_method
        size: 2
      - id: last_mod_time
        size: 2
      - id: last_mod_date
        size: 2
      - id: crc32
        size: 4
      - id: compressed_size
        size: 4
      - id: uncompressed_size
        size: 4
      - id: file_name_length
        size: 2
      - id: extra_field_length
        size: 2
    instances:
      file_name:
        type: str
        size: file_name_length
        encoding: UTF-8
      extra_field:
        size: extra_field_length