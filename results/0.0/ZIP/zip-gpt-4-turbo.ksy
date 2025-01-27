meta:
  id: zip
  file-extension: zip
  endian: le
  title: ZIP Archive
  license: CC0-1.0
  ks-version: 0.9

doc: |
  ZIP is a popular archive file format that is widely used. The format supports various compression methods and has a structure that can include multiple files and directories.

seq:
  - id: sections
    type: section
    repeat: eos

types:
  section:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x03, 0x04]
      - id: version
        type: u2
      - id: flags
        type: u2
      - id: compression
        type: u2
      - id: file_mod_time
        type: u2
      - id: file_mod_date
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