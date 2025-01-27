meta:
  id: zip
  title: ZIP Archive
  file-extension: zip
  license: MIT
  endian: le
seq:
  - id: signatures
    type: local_file_headers
    repeat: eos
types:
  local_file_headers:
    seq:
      - id: header
        type: local_file_header
  local_file_header:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x03, 0x04]
      - id: version_needed_to_extract
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
        encoding: UTF-8
      - id: extra_field
        size: extra_field_length
      - id: body
        size: compressed_size