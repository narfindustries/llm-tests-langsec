meta:
  id: zip
  file-extension: zip
  endian: le
  imports:
    - ../../file_formats/compressed/lz77.ksy
seq:
  - id: local_file_header
    type: local_file_header
  - id: file_data
    size: local_file_header.compressed_size
    process: lz77
  - id: data_descriptor
    type: data_descriptor
    if: local_file_header.flags.has_data_descriptor
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
      - id: file_name
        size: file_name_length
        type: str
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
  data_descriptor:
    seq:
      - id: crc32
        size: 4
      - id: compressed_size
        size: 4
      - id: uncompressed_size
        size: 4