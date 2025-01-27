meta:
  id: zip
  file-extension: zip
  endian: le
seq:
  - id: signature
    contents: [0x50, 0x4b, 0x03, 0x04]
  - id: version_extract
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
  - id: filename_length
    type: u2
  - id: extra_field_length
    type: u2
  - id: filename
    type: str
    size: filename_length
    encoding: UTF-8
  - id: extra_field
    size: extra_field_length
  - id: compressed_data
    size: compressed_size