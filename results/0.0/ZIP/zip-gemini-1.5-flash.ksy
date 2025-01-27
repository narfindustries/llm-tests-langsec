meta:
  id: zip-gemini-1
  title: "ZIP Gemini 1.5 Flash"
  homepage: ""
  file-extension: ".zip"
  endian: be

seq:
  - id: header
    type: header
  - id: central_directory
    type: central_directory

types:
  header:
    seq:
      - id: signature
        type: u4
        enum:
          0x04034b50: local_file_header
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
      - id: filename_length
        type: u2
      - id: extra_field_length
        type: u2
      - id: filename
        type: str
        size: filename_length
      - id: extra_field
        type: bytes
        size: extra_field_length
      - id: file_data
        type: bytes
        size: compressed_size

  central_directory:
    seq:
      - id: signature
        type: u4
        enum:
          0x02014b50: central_directory_entry
      - id: version
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
      - id: filename_length
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
      - id: local_header_offset
        type: u4
      - id: filename
        type: str
        size: filename_length
      - id: extra_field
        type: bytes
        size: extra_field_length
      - id: file_comment
        type: str
        size: file_comment_length

