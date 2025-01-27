meta:
  id: zip_deepseek_chat
  file-extension: zip
  endian: le
  license: MIT
  ks-version: 0.9

seq:
  - id: local_file_header
    type: local_file_header
    repeat: eos

types:
  local_file_header:
    seq:
      - id: signature
        contents: "PK\x03\x04"
      - id: version_needed
        type: u2
      - id: general_purpose_bit_flag
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
      - id: file_name_length
        type: u2
      - id: extra_field_length
        type: u2
      - id: file_name
        type: str
        size: file_name_length
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length