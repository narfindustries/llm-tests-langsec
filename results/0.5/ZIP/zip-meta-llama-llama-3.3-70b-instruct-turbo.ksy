meta:
  id: zip
  file-extension: zip
  endian: le
  title: Zip Archive
  license: MIT

seq:
  - id: local_file_header
    type: local_file_header
    repeat: until eof
    doc: local file header

types:
  local_file_header:
    seq:
      - id: magic
        type: u2
        doc: must be 0x04034b50
      - id: version_needed_to_extract
        type: u2
        doc: version needed to extract
      - id: general_purpose_bit_flag
        type: u2
        doc: general purpose bit flag
      - id: compression_method
        type: u2
        doc: compression method (0 = stored, 1-7 = various compression algorithms)
      - id: last_mod_time
        type: u2
        doc: last modification time
      - id: last_mod_date
        type: u2
        doc: last modification date
      - id: crc_32
        type: u4
        doc: crc-32
      - id: compressed_size
        type: u4
        doc: compressed size
      - id: uncompressed_size
        type: u4
        doc: uncompressed size
      - id: file_name_length
        type: u2
        doc: file name length
      - id: extra_field_length
        type: u2
        doc: extra field length
      - id: file_name
        type: str
        size: file_name_length
        encoding: UTF-8
        doc: file name
      - id: extra_field
        type: str
        size: extra_field_length
        doc: extra field
      - id: data
        type: str
        size: compressed_size
        doc: file data