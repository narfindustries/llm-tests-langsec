meta:
  id: zip
  title: ZIP archive
  file-extension: zip
  endian: le
seq:
  - id: signatures
    type: signatures
  - id: central_dir
    type: central_dir
types:
  signatures:
    seq:
      - id: first_signature
        contents: [0x50, 0x4b, 0x03, 0x04]
      - id: local_file_headers
        type: local_file_header
        repeat: until
        repeat-until: not _io.is_eof()
  local_file_header:
    seq:
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
        encoding: utf-8
      - id: extra_field
        size: extra_field_length
      - id: body
        size: compressed_size
  central_dir:
    seq:
      - id: central_dir_signature
        contents: [0x50, 0x4b, 0x01, 0x02]
      - id: central_dir_entries
        type: central_dir_entry
        repeat: until
        repeat-until: not _io.is_eof()
  central_dir_entry:
    seq:
      - id: version_made_by
        type: u2
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
      - id: file_name
        type: str
        size: file_name_length
        encoding: utf-8
      - id: extra_field
        size: extra_field_length
      - id: file_comment
        type: str
        size: file_comment_length
        encoding: utf-8