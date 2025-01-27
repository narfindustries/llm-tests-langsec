meta:
  id: zip
  title: ZIP file format
  file-extension: zip
  endian: le
  encoding: UTF-8

seq:
  - id: header
    type: zip_local_file_header

types:
  zip_local_file_header:
    seq:
      - id: signature
        type: u4
      - id: version
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
        size: file_name_length
      - id: extra_field
        type: str
        size: extra_field_length

  zip_data_descriptor:
    seq:
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4