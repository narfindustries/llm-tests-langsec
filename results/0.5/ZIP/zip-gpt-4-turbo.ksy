meta:
  id: zip
  file-extension: zip
  endian: le
  title: ZIP Archive
  license: CC0-1.0
  ks-version: 0.9

doc: |
  ZIP is a popular file compression and archive format. This Kaitai Struct specification
  describes the structure of a ZIP file.

seq:
  - id: sections
    type: section
    repeat: eos

types:
  section:
    seq:
      - id: local_file_header
        type: local_file_header
      - id: body
        size: local_file_header.compressed_size
      - id: data_descriptor
        type: data_descriptor
        if: local_file_header.general_purpose_bit_flag & 0b00001000 != 0

  local_file_header:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x03, 0x04]
      - id: version_needed_to_extract
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

  data_descriptor:
    seq:
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4